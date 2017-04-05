use strict;
use warnings;

my $filename = $ARGV[0] || 'HDT-data3.tsv';

# I honestly don't know what the encoding here is, it looks like it's been re-encoded a
# couple times, because it ends up as ??[symbol], which seems roundtripped through
# something because there shouldn't be two question marks. We strip out any non-ascii
# alphanum, so it doesn't particularly matter in the grand scheme of things.
open(my $input, '<:encoding(latin1)', $filename) or die "Cannot open $filename for read: $!";

my $ignored = do {
  my @ignored = qw(a the for in and or is in are of);
  my $joined = join('|', map { quotemeta } @ignored);
  qr/\b(?:$joined)\b/;
};

my %keywords;
my @articles;
my $sum_log_views;
# Skip header
<$input>;

sub update_keywords {
  my ($kw1, $kw2, $type, $pageviews, $log_views, $article_id) = @_;
  my $word = join("\t", $kw1, $kw2, $type);
  my $keywords = ($keywords{$word} ||= { count => 0, pageviews => 0, log_pageviews => 0, min_pageviews => $log_views, max_pageviews => $log_views, occurances => [] });
  $keywords->{count}++;
  $keywords->{pageviews} += $pageviews;
  $keywords->{log_pageviews} += $log_views;
  $keywords->{max_pageviews} = $log_views if $log_views > $keywords->{max_pageviews};
  $keywords->{min_pageviews} = $log_views if $log_views < $keywords->{min_pageviews};
  push(@{$keywords->{occurances}}, $article_id);
  return $keywords;
}

while(<$input>) {
  chomp;
  my ($title, $url, $date, $pageviews) = split(/\t/, lc);
  my $log_views = log(1 + $pageviews);
  # Replace any non-alphanumeric characters with a space
  $title =~ s/[^a-z0-9]/ /g;
  # Remove words we ignore
  $title =~ s/$ignored/ /g;
  # Turn consecutive spaces into a single space
  $title =~ s/ +/ /g;
  # Remove leading and trailing whitespace
  $title =~ s/^ //;
  $title =~ s/ $//;
  my $type = ($url =~ /blogs/) ? 'BLOG' : 'OTHER';
  push(@articles, [$., $title, $log_views, "BAD"]);
  my @keywords = split(/ +/, $title);
  for my $kw (@keywords) {
    my $word = "$kw\tN/A\t$type";
    update_keywords($kw, 'N/A', $type, $pageviews, $log_views, $#articles);
  }
  for my $id (0 .. $#keywords - 1) {
    update_keywords($keywords[$id], $keywords[$id+1], $type, $pageviews, $log_views, $#articles);
  }
  $sum_log_views += $log_views;
}

close $input;

my $average_views = $sum_log_views / @articles;

open(my $output, '>:encoding(utf-8)', 'hdt-out.txt');
open(my $reasons, '>:encoding(utf-8)', 'hdt-reasons.txt');

print $output "Word 1\tWord 2\tType\tOccurances\tAverage Pageviews (log)\tMinimum Pageviews (log)\tMaximum Pageviews (log)\tArticles\n";
print $reasons "Keywords\tPage Views (log)\tWord 1\tWord 2\tType\tOccurances\tAverage Pageviews (log)\tMinimum Pageviews (log)\tMaximum Pageviews (log)\n";

my $good_keywords = 0;
while (my ($word, $info) = each %keywords) {
  my ($count, $views, $min, $max, $occurances) = @{$info}{qw(count log_pageviews min_pageviews max_pageviews occurances)};
  my @occurances = @$occurances;
  my $avg = $views / $count;
  if (($count >=  4 && $count <   8 && $min > 6.9 && $avg > 7.6) ||
      ($count >=  8 && $count <  16 && $min > 6.7 && $avg > 7.4) ||
      ($count >= 16 && $count < 200 && $min > 6.1 && $avg > 6.2)) {
    local $" = "~";
    my $ids = join('~', map { $articles[$_][0] } @occurances);
    print $output "$word\t$count\t$avg\t$min\t$max\t$ids\n";
    $good_keywords++;
    for my $article (map { $articles[$_] } @occurances) {
      my ($id, $title, $views, $status) = @$article;
      print $reasons "$title\t$views\t$word\t$count\t$avg\t$min\t$max\n";
      $article->[3] = 'GOOD' if $status eq 'BAD';
    }
  }
}

close $output;
close $reasons;

my $threshold = 7.1;
my ($good_articles, $bad_articles, $good_views, $bad_views, $real_good, $real_bad, $false_positive, $false_negative);

for my $article (@articles) {
  my ($id, $title, $views, $status) = @$article;
  if ($status eq 'GOOD') {
    $good_articles++;
    $good_views += $views;
    $false_positive++ if $views < $threshold;
  } else {
    $bad_articles++;
    $bad_views += $views;
    $false_negative++ if $views > $threshold;
  }
  $real_good++ if $views > $threshold;
}

$real_bad = @articles - $real_good;

my $num_articles = @articles;
my $num_keywords = keys %keywords;
my $avg_good = $good_views / $good_articles;
my $avg_bad = $bad_views / $bad_articles;
my $error_rate = ($false_positive + $false_negative) / $num_articles;
my $aggregation_factor = ($num_keywords / $good_keywords) / ($num_articles / $good_articles);

print "Average pageviews: $average_views\n";
print "Number of articles marked as good: $good_articles (real number is $real_good)\n";
print "Number of articles marked as bad: $bad_articles (real number is $real_bad)\n";
print "Avg pv: articles marked as good: $avg_good\n";
print "Avg pv: articles marked as bad: $avg_bad\n";
print "Number of false positive: $false_positive (bad marked as good)\n";
print "Number of false negative: $false_negative (good marked as bad)\n";
print "Number of articles: $num_articles\n";
print "Error Rate: $error_rate\n";
print "Number of feature values: $num_keywords (marked as good: $good_keywords)\n";
print "Aggregation factor: $aggregation_factor\n";
