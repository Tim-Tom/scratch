use strict;
use warnings;

my $filename = $ARGV[0] || 'HDT-data3.tsv';

# Set up a regular expression that will match any keyword in a title that we should ignore
my $ignored = do {
  my @ignored = qw(a the for in and or is in are of);
  my $joined = join('|', map { quotemeta } @ignored);
  qr/\b(?:$joined)\b/;
};

my %keywords;
my @articles;
my $sum_log_pageviews;

sub update_keywords {
  my ($kw1, $kw2, $type, $pageviews, $log_pageviews, $article_id) = @_;
  my $word = join("\t", $kw1, $kw2, $type);
  my $keywords = ($keywords{$word} ||= { count => 0, pageviews => 0, log_pageviews => 0, min_pageviews => $log_pageviews, max_pageviews => $log_pageviews, occurances => [] });
  $keywords->{count}++;
  $keywords->{pageviews} += $pageviews;
  $keywords->{log_pageviews} += $log_pageviews;
  $keywords->{max_pageviews} = $log_pageviews if $log_pageviews > $keywords->{max_pageviews};
  $keywords->{min_pageviews} = $log_pageviews if $log_pageviews < $keywords->{min_pageviews};
  push(@{$keywords->{occurances}}, $article_id);
  return $keywords;
}

##
# -Part 1-
# Read in the tab seperated data file, normalize it, and save off relevant information in
# %keywords and @articles.
##

# I honestly don't know what the encoding here is, it looks like it's been re-encoded a
# couple times, because it ends up as ??[symbol], which seems roundtripped through
# something because there shouldn't be two question marks. We strip out any non-ascii
# alphanum, so it doesn't particularly matter in the grand scheme of things. I went with
# CP-1252 because that's the default encoding in windows and this looks to be a windows
# text file.
open(my $input, '<:encoding(CP-1252)', $filename) or die "Cannot open $filename for read: $!";

# Skip header
<$input>;

while(<$input>) {
  chomp;
  my ($title, $url, $date, $pageviews) = split(/\t/, lc);
  my $log_pageviews = log(1 + $pageviews);
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
  push(@articles, [$., $title, $log_pageviews, "BAD"]);
  my @keywords = split(/ /, $title);
  for my $kw (@keywords) {
    my $word = "$kw\tN/A\t$type";
    update_keywords($kw, 'N/A', $type, $pageviews, $log_pageviews, $#articles);
  }
  for my $id (0 .. $#keywords - 1) {
    update_keywords($keywords[$id], $keywords[$id+1], $type, $pageviews, $log_pageviews, $#articles);
  }
  $sum_log_pageviews += $log_pageviews;
}
close $input;

##
# - Part 2 -
# Categorize articles as good or bad based on keyword pageviews.
##

open(my $output, '>:encoding(ascii)', 'hdt-out.tsv');
open(my $reasons, '>:encoding(ascii)', 'hdt-reasons.tsv');

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

##
# - Part 3 -
# Validate the methodology used above and print out summary statistics.
##

my $threshold = 7.1;
my ($good_articles, $bad_articles, $good_pageviews, $bad_pageviews, $real_good, $real_bad, $false_positives, $false_negatives);

for my $article (@articles) {
  my ($id, $title, $views, $status) = @$article;
  if ($status eq 'GOOD') {
    $good_articles++;
    $good_pageviews += $views;
    $false_positives++ if $views < $threshold;
  } else {
    $bad_articles++;
    $bad_pageviews += $views;
    $false_negatives++ if $views > $threshold;
  }
  $real_good++ if $views > $threshold;
}

$real_bad = @articles - $real_good;

my $num_articles = @articles;
my $num_keywords = keys %keywords;
my $avg_pageviews = $sum_log_pageviews / @articles;
my $avg_good = $good_pageviews / $good_articles;
my $avg_bad = $bad_pageviews / $bad_articles;
my $error_rate = ($false_positives + $false_negatives) / $num_articles;
my $aggregation_factor = ($num_keywords / $good_keywords) / ($num_articles / $good_articles);

print <<"END_SUMMARY";
Average pageviews: $avg_pageviews
Number of articles marked as good: $good_articles (real number is $real_good)
Number of articles marked as bad: $bad_articles (real number is $real_bad)
Avg pv: articles marked as good: $avg_good
Avg pv: articles marked as bad: $avg_bad
Number of false positives: $false_positives (bad marked as good)
Number of false negatives: $false_negatives (good marked as bad)
Number of articles: $num_articles
Error Rate: $error_rate
Number of feature values: $num_keywords (marked as good: $good_keywords)
Aggregation factor: $aggregation_factor
END_SUMMARY
