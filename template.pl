use strict;
use warnings;

use v5.24;

no warnings 'experimental::signatures';
use feature 'signatures';

no warnings 'experimental::postderef';
use feature 'postderef';

use File::Slurp qw(read_file);
use Unicode::UTF8 qw(decode_utf8);

use Const::Fast;
use YAML::XS;

const my $done => 0;
const my $in_text => 1;
const my $in_code => 2;
const my $in_do => 3;
const my $in_do_escape => 4;

my %config = YAML::XS::Load(decode_utf8(scalar read_file('template.yml', { binmode => ':raw' })))->%*;
my $text = decode_utf8(scalar read_file('template.epl', {binmode => ':raw' }));
my $cur_text = $text;
my $buffer = '';
my $strip = 0;

my $line_no = 1;

my $state = $in_text;

my $sym = 0;
my $package = $config{package} || ('Template::EmbeddedPerl::GEN' . ++$sym);
my $sub_text = "package $package;\nuse strict;\nuse warnings;\nsub {\nmy \$OUT = q##;\n";
if (exists $config{variables}) {
  $sub_text .= 'my %variables = @_;'."\n";
  for my $variable ($config{variables}->@*) {
    my $sigil = substr($variable, 0, 1);
    my $sigilless = substr($variable, 1);
    if ($sigil eq '$') {
      $sub_text .= "my $variable = \$variables{$sigilless};\n"
    } elsif ($sigil eq '@') {
      $sub_text .= "my $variable = \@{\$variables{$sigilless} || []};\n"
    } elsif ($sigil eq '%') {
      $sub_text .= "my $variable = \%{\$variables{$sigilless} || {}};\n"
    }
  }
}

sub insert_text {
  return unless $buffer;
  $buffer =~ s/\\/\\/g;
  $buffer =~ s/([#$@%])/\$1/g;
  $line_no += $buffer =~ s/\n/\\n/g;
  $sub_text .= '$OUT .= qq#' . $buffer . "#;\n";
  $buffer = '';
}

sub escape {
  return $_[0];
}

while($state != $done) {
  if ($state == $in_text) {
    my $index = index($cur_text, '<%');
    if ($index == -1) {
      $buffer .= $cur_text;
      $state = $done;
      last;
    }
    if (substr($cur_text, $index + 2, 1) eq '%') {
      $buffer .= substr($cur_text, 0, $index + 2);
      $cur_text = substr($cur_text, $index + 3);
    } else {
      $buffer .= substr($cur_text, 0, $index);
      $index += 2;
      if (substr($cur_text, $index, 1) eq '`') {
        $strip = 1;
        ++$index;
      }
      my $variant = substr($cur_text, $index, 1);
      if ($variant eq '-') {
        $state = $in_do;
        ++$index;
      } elsif ($variant eq '=') {
        $state = $in_do_escape;
        ++$index;
      } else {
        $state = $in_code;
      }
      $cur_text = substr($cur_text, $index);
    }
  } else {
    $buffer =~ s/[ \t]*$// if $strip;
    insert_text;
    my $index = index($cur_text, '%>');
    if ($index == -1) {
      # This is broken now, may have to go back to global indexes :(
      my $text_so_far = substr($text, 0, $index);
      my $line = ($text_so_far =~ tr/\n/\n/);
      my $char = length($text_so_far) - rindex($text_so_far, "\n");
      warn "sub: $sub_text";
      warn "buffer: $buffer";
      die "Unexpected end of input in code block started at line $line character $char";
    }
    my $line = "#line $line_no template.epl";
    my $code_text = substr($cur_text, 0, $index);
    $line_no += ($code_text =~ tr/\n/\n/);
    $code_text =~ s/^\s+//;
    $code_text =~ s/\s+$//;
    if ($state == $in_do) {
      $sub_text .= "\$OUT .= do {\n$line\n$code_text\n};\n";
    } elsif ($state == $in_do_escape) {
      $sub_text .= "\$OUT .= escape do {\n$line\n$code_text\n};\n";
    } else {
      $sub_text .= "$line\n$code_text\n";
    }
    $cur_text = substr($cur_text, $index + 2);
    if ($strip) {
      $cur_text =~ /^[ \t]*\n?/;
      if ($+[0]) {
        if (substr($cur_text, $+[0] - 1, 1) eq "\n") {
          $line_no += 1;
        }
        $cur_text = substr($cur_text, $+[0]);
      }
      $strip = 0;
    }
    $state = $in_text;
  }
}

insert_text;

$sub_text .= "return \$OUT;\n}\n";

say $sub_text;

my $sub = eval $sub_text;
say '***' . $sub->(count => 5, values => [qw(a b c)]) . '***';
