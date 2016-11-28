use strict;
use warnings;

use v5.22;

use utf8;

use Getopt::Long;
use Pod::Usage;

use File::Temp qw(tempfile);
use File::Copy qw(move);

=pod

=head1 NAME

Clean UTF-8 - Detects or fixes unicode characters in input files.

=head1 SYNOPSIS

perl clean-utf8.pl [--delete] [--fix] files

=head1 DESCRIPTION

The UTF cleaning script detects, deletes, or "fixes" unicode characters in the specified
files.

-head1 OPTIONS

All options can be shortened to their leading character.

=over

=item --delete

Removes any unicode characters it finds instead of deleting them.

=item --fix

Attempts to downgrade the uncicode characters to their ascii variants.

=item --partial

If only fix is specified, specifying the partial argument will replace the input files
even if a fix for all the characters encountered is not discovered.

=back

=cut

sub clean {
  return scalar s/[^[:ascii:]]//g;
}

sub fix {
  tr/Åáâäåçèéêíñóôöûü/Aaaaaceeeinooouu/;
}

my (%totals, %byFile);

my %args;
GetOptions("help"    => \$args{help},
           "fix"     => \$args{fix},
           "delete"  => \$args{delete},
           "partial" => \$args{partial}) or pod2usage(2);

pod2usage(-exit_val => 0, -verbose => 2) if $args{help};

pod2usage(-exit_val => 1, -message => "Files are required") unless @ARGV;

my $doOutput = $args{fix} || $args{delete};

my $globalError;

binmode(*STDOUT, ':encoding(utf-8)');

foreach my $filename (@ARGV) {
  open(my $in, '<:encoding(utf-8)', $filename) or die "Cannot open '$filename' for input: $!";
  my ($out, $outname, $changed, $error);
  if ($doOutput) {
    ($out, $outname) = tempfile();
    binmode($out, ':encoding(utf-8)');
  }
  while(<$in>) {
    my $bad;
    while (/([^[:ascii:]])/g) {
      ++$totals{$1};
      ++$byFile{$1}{$filename};
      $bad = 1;
    }
    if ($bad) {
      if ($args{fix}) {
        my $fixed = fix();
        $changed ||= $fixed;
      }
      if ($args{delete}) {
        my $cleaned = clean();
        $changed ||= $cleaned;
      }
      $error ||= (/[^[:ascii:]]/);
    }
    print $out $_ if $doOutput;
  }
  $globalError ||= $error;
  if ($error) {
    if (!$doOutput) {
      say "Unicode characters discovered in file '$filename'";
    } else {
      if ($args{partial}) {
        say "Unicode characters discovered in file '$filename': Warning : Some remain";
      } else {
        say "Unicode characters discovered in file '$filename': Warning : Could not fix all characters, file left alone";
      }
    }
  } elsif ($changed) {
    say "Unicode characters discovered in file '$filename'";
  }
  if ($doOutput) {
    close $out;
    if ($changed && (!$error || $args{partial})) {
      move($outname, $filename) or die "Cannot replace '$filename' with updated contents: $!";
    } else {
      unlink $outname;
    }
  }
}

foreach my $c (sort keys %totals) {
  say "$c:";
  say "\tTotal: $totals{$c}";
  say "\t$_: $byFile{$c}{$_}" foreach keys %{$byFile{$c}};
}

exit 2 if $globalError;
