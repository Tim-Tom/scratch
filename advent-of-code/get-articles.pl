use strict;
use warnings;

use v5.24;

use Mojo::UserAgent;

my $ua = Mojo::UserAgent->new;

my $year = $ARGV[0] // 2017;

mkdir 'articles' unless -d 'articles';
mkdir "articles/$year" unless -d "articles/$year";

for my $day (1 .. 25) {
  my $filename = sprintf 'articles/%04d/Day-%02d.html', $year, $day;
  next if -f $filename;
  say "Downloading article for day $day";
  open(my $output, '>:encoding(utf-8)', $filename);
  my $article = $ua->get("http://adventofcode.com/$year/day/$day")->result->dom(q(article > *))->join("\n  ");
  print $output <<"END_ARTICLE";
<html>
<head>
  <title>Advent $year Day $day</title>
</head>
<body>
  $article
</body>
END_ARTICLE
}

for my $day (1 .. 25) {
  my ($html, $text) = map { sprintf 'articles/%04d/Day-%02d.%s', $year, $day, $_ } qw(html txt);
  next if -f $text;
  die if -z $html;
  say "Converting article for day $day to text";
  system(qq(html2text -utf8 <$html >$text));
}
