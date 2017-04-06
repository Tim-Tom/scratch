use v6;

enum Status <GOOD BAD>;

class Article {
    has Int    $.id            is required;
    has Str    $.title         is required;
    has Num    $.pageviews-log is required;
    has Str    @.keywords      is required;
    has Status $.status        is rw = BAD;
}

enum KeywordType <BLOG OTHER>;

class Keyword {
    has Str         $.kw1           is required;
    has Str         $.kw2           is required;
    has KeywordType $.type          is required;
    has Int         $.count         is rw;
    has Int         $.pageviews     is rw;
    has Num         $.pageviews-log is rw;
    has Num         $.pageviews-max is rw;
    has Num         $.pageviews-min is rw;
    has Article     @.occurances;
}

my $filename = @*ARGS[0] // 'HDT-data3.tsv';

my $ignored = set(<a the for in and or is in are of>);

my Keyword %keywords;
my Article @articles;
my Num $sum-log-pageviews;

sub update-keyword($kw1, $kw2, $type, $pageviews, $pageviews-log, $article) {
  my $key = [$kw1, $kw2, $type].join("\t");
  my $keyword = (%keywords{$key} //= Keyword.new(:$kw1, :$kw2, :$type));
  $keyword.count++;
  $keyword.pageviews += $pageviews;
  $keyword.pageviews-log += $pageviews-log;
  $keyword.pageviews-max max= $pageviews-log;
  $keyword.pageviews-min min= $pageviews-log;
  $keyword.occurances.push($article);
  return $keyword;
}

##
# -Part 1-
# Read in the tab seperated data file, normalize it, and save off relevant information in
# %keywords and @articles.
##

{
    # I honestly don't know what the encoding here is, it looks like it's been re-encoded
    # a couple times, because it ends up as ??[symbol], which seems roundtripped through
    # something because there shouldn't be two question marks. We strip out any non-ascii
    # alphanum, so it doesn't particularly matter in the grand scheme of things. I went
    # with latin1 because that's the closest equivalent perl6 supports for the default
    # CP-1252 encoding in used in windows and this looks to be a windows text file.
    my $input = $filename.IO.open(:enc<latin1>, :r);

    for $input.lines.kv -> $line-no, $line {
        # Ignore header
        FIRST { next; }
        my ($title, $url, $date, $pageviews) = $line.lc.split(/\t/);
        my $pageviews-log = log(1 + $pageviews);
        my @keywords = $title.comb(/\w+/).grep(* ∉ $ignored);
        my KeywordType $type = ($url ~~ /blogs/) ?? BLOG !! OTHER;
        my Article $article .= new(id => $line-no + 1, :$title, :@keywords, :$pageviews-log);
        @articles.push($article);
        $sum-log-pageviews += $pageviews-log;
        for @keywords -> $kw {
            update-keyword($kw, 'N/A', $type, $pageviews, $pageviews-log, $article);
        }
        for @keywords Z, @keywords[1..*] -> [$kw1, $kw2] {
            update-keyword($kw1, $kw2, $type, $pageviews, $pageviews-log, $article);
        }
    }
}

##
# - Part 2 -
# Categorize articles as good or bad based on keyword pageviews.
##

my $good-keywords = 0;
{
    my $output = 'hdt-out.tsv'.IO.open(:w, :enc<utf-8>);
    my $reasons = 'hdt-reasons.tsv'.IO.open(:w, :enc<utf-8>);
    $output.put("Word 1\tWord 2\tType\tOccurances\tAverage Pageviews (log)\tMinimum Pageviews (log)\tMaximum Pageviews (log)\tArticles");
    $reasons.put("Keywords\tPage Views (log)\tWord 1\tWord 2\tType\tOccurances\tAverage Pageviews (log)\tMinimum Pageviews (log)\tMaximum Pageviews (log)");

    for %keywords.kv -> $word, $keyword {
        my $average = $keyword.pageviews-log / $keyword.count;
        if  4 <= $keyword.count <   8 && $keyword.pageviews-min > 6.9 && $average > 7.6 ||
            8 <= $keyword.count <  16 && $keyword.pageviews-min > 6.7 && $average > 7.4 ||
           16 <= $keyword.count < 200 && $keyword.pageviews-min > 6.1 && $average > 6.2 {
            my $ids = $keyword.occurances>>.id.join("~");
            $output.put("$word\t{$keyword.count}\t$average\t{$keyword.pageviews-min}\t{$keyword.pageviews-max}\t$ids");
            ++$good-keywords;
            for $keyword.occurances -> $article {
                $reasons.put("{$article.keywords.join(' ')}\t{$article.pageviews-log}\t$word\t{$keyword.count}\t$average\t{$keyword.pageviews-min}\t{$keyword.pageviews-max}");
                $article.status = GOOD if $article.status == BAD;
            }
        }
    }
}

##
# - Part 3 -
# Validate the methodology used above and print out summary statistics.
##

my $threshold = 7.1;
my ($good-articles, $bad-articles, $good-pageviews, $bad-pageviews, $real-good, $real-bad, $false-positives, $false-negatives);

for @articles -> $article {
    if $article.status == GOOD {
        $good-articles++;
        $good-pageviews += $article.pageviews-log;
        $false-positives++ if $article.pageviews-log < $threshold;
    } else {
        $bad-articles++;
        $bad-pageviews += $article.pageviews-log;
        $false-negatives++ if $article.pageviews-log > $threshold;
    }
    $real-good++ if $article.pageviews-log > $threshold;
}

$real-bad = @articles - $real-good;

say qq:to<END_SUMMARY>;
Average pv: { $sum-log-pageviews / @articles }
Number of articles marked as good: $good-articles (real number is $real-good)
Number of articles marked as bad: $bad-articles (real number is $real-bad)
Avg pv: articles marked as good: { $good-pageviews / $good-articles }
Avg pv: articles marked as bad: { $bad-pageviews / $bad-articles }
Number of false positives: $false-positives (bad marked as good)
Number of false negatives: $false-negatives (good marked as bad)
Number of articles { +@articles }
Error Rate: { ($false-positives + $false-negatives) / @articles }
Number of feature values: { +%keywords } (marked as good: { $good-keywords })
Aggregation factor: { (%keywords / $good-keywords) / (@articles / $good-articles) }
END_SUMMARY
