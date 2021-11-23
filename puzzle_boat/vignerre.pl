use strict;
use warnings;

use utf8;

use v5.20;

chomp(my @clues = <DATA>);

my @alphabet = 'A'..'Z';
my %cipher;
{
  my @letters = @alphabet;
  for my $l (@alphabet) {
    $cipher{$l} = {map { $letters[$_] => chr($_ + ord 'A')} keys @letters};
    @letters = (@letters[1..$#letters], $letters[0]);
  }
}
use Data::Printer;

my @keys = qw(
MALTA
BELGIUM
SWEDEN
OXYGEN
HUNGARY
GERMANY
SPAIN
NORWAY
URANIUM
ITALY
FRANCE
PHOSPHORUS
SULFUR
FLUORINE
YTTRIUM
BORON
POTASSIUM
HYDROGEN
PORTUGAL
AUSTRIA
VATICANCITY
CARBON
TUNGSTEN
IODINE
NITROGEN
VANADIUM
);

for my $i (keys @clues) {
  my $clue = $clues[$i];
#  for my $key (@keys) {
  #  my $key = 'BASIL';
  my $key = @keys[$i];
  my @key = split(//, $key);
  my $result = '';
  my $index = 0;
  for my $letter (split(//, $clue)) {
    my $trans = $cipher{$key[$index]}{$letter};
    if (defined $trans) {
      $result .=  $trans;
      $index = ($index + 1) % @key;
    } else {
      $result .= $letter;
    }
  }
  say "$clue\t$result\t$key";
}

__DATA__
ANP XLQMPGT
ULP YCVEFUFKVN
GN IOIZWJX
RLL’Z ABFOW
ABR LOCJVQVTG
SMJEIAE ISLZTEW
KTT OVNTN
RZVIELG OEZ
OJE GPY
AHMPFWP AANMTRD
HCUR UXFITF
BVFW GLCIXWGPBY
XCCXN GSLE
YSYFV’A AS
RAX EMRF
HVFIYJGYZL BRFFNCZV
WOOE LG
JJXV DNVNZCV
AWTXHYE AAOKXM
IXWGKQFY NZX
IIGMVERP ABVOY-MPTER
NEKUSEU MRUQUKNX
RIH CAEP
NWUAG VMCULRV
BV XLFUTRNV
JR PHHUCOVL
