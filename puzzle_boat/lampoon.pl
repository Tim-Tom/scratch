use strict;
use warnings;

use v5.26;

use experimental qw(signatures);

no warnings 'deprecated';

use List::Util qw(sum);
use Data::Printer;
use Carp qw(confess);

my $WIDTH = 21;
my $HEIGHT = 25;

my %cities;
my %countries;

while(<DATA>) {
  chomp;
  my ($city, $country, @intersects) = split(/\t/);
  my %intersects;
  $cities{$city} = {
    city => $city,
    country => $country,
    intersects => [@intersects],
  };
  push($countries{$country}->@*, $city);
}
close *DATA;

my $world = {
  used   => {},
  grid   => [map { [(' ', ) x $WIDTH] } 1 .. $HEIGHT],
  pick   => [map { [(undef, ) x $WIDTH] } 1 .. $HEIGHT],
  remain => {}
 };

for my $city (values %cities) {
  my %remain;
  $remain{$_}++ foreach ($city->{intersects}->@*);
  $world->{remain}{$city->{city}} = \%remain;
}

my @backups;

sub backup {
  my $copy = {
    used   => { $world->{used}->%* },
    grid   => [ map { [@$_] } $world->{grid}->@* ],
    pick   => [ map { [@$_] } $world->{pick}->@* ],
    remain => { map { $_ => {$world->{remain}{$_}->%*} } keys $world->{remain}->%* }
   };
  push(@backups, $copy);
  return;
}

sub restore {
  $world = pop @backups;
  confess "Restore without backup" unless defined $world;
  return;
}

sub discard {
  pop @backups;
  return;
}

sub print_grid {
  for my $row (keys $world->{grid}->@*) {
    say join('', $world->{grid}[$row]->@*);
  }
}


my $false = 0;
our $depth = 0;

sub insert_vertical($city, $row, $col) {
  return 0 if $world->{used}{$city};
  return 0 if $row < 0;
  return 0 if $row + length $city > $HEIGHT;
  my @letters = split(//, $city);
  my @changed;
  backup;
  if ($row > 0) {
    if ($world->{grid}[$row - 1][$col] eq ' ') {
      $world->{grid}[$row - 1][$col] = '.';
    } elsif ($world->{grid}[$row - 1][$col] ne '.') {
      restore;
      return 0;
    }
  }
  if ($row + length $city < $HEIGHT) {
    if ($world->{grid}[$row + length $city][$col] eq ' ') {
      $world->{grid}[$row + length $city][$col] = '.';
    } elsif ($world->{grid}[$row + length $city][$col] ne '.') {
      restore;
      return 0;
    }
  }
  $world->{used}{$city} = 1;
  say ' 'x$depth . "IV: $city [$row, $col]";
  local $depth = $depth + 1;
  for my $i (keys @letters) {
    if ($world->{grid}[$row + $i][$col] eq ' ') {
      $world->{grid}[$row + $i][$col] = $letters[$i];
      $world->{pick}[$row + $i][$col] = $city;
      push(@changed, [$letters[$i], $row + $i]);
    } elsif($world->{grid}[$row + $i][$col] eq $letters[$i]) {
      my $cross = $world->{pick}[$row + $i][$col];
      confess unless $cross;
      if ($world->{remain}{$city}{$cities{$cross}{country}} && $world->{remain}{$cross}{$cities{$city}{country}}) {
        --$world->{remain}{$city}{$cities{$cross}{country}};
        --$world->{remain}{$cross}{$cities{$city}{country}};
      } else {
        restore;
        return 0;
      }
    } else {
      restore;
      return 0;
    }
  }
  my ($changed_i, $country_i, $city_i);
  my @countries = sort keys $world->{remain}{$city}->%*;
  my @index_backup;
 iv_changed:
  for ($changed_i = 0; $changed_i < @changed; ++$changed_i) {
    my ($letter, $r) = $changed[$changed_i]->@*;
    for ($country_i = 0; $country_i < @countries; ++$country_i) {
      my @cities = $countries{$countries[$country_i]}->@*;
      next unless $world->{remain}{$city}{$countries[$country_i]};
      for ($city_i = 0; $city_i < @cities; ++$city_i) {
        my $citi = $cities[$city_i];
        next if $world->{used}{$citi};
        next unless $world->{remain}{$citi}{$cities{$city}{country}};
        my $last_index = 0;
        my $index;
        while(($index = index($citi, $letter, $last_index)) != -1) {
          if (insert_horizontal($citi, $r, $col - $index)) {
            push(@index_backup, [$changed_i, $country_i, $city_i, $index]);
            next iv_changed;
          iv_bad_choice:
            ($changed_i, $country_i, $city_i, $index) = shift(@index_backup)->@*;
            ($letter, $r) = $changed[$changed_i]->@*;
            @cities = $countries{$countries[$country_i]}->@*;
            $citi = $cities[$city_i];
          }
          $last_index = $index + 1;
        }
      }
    }
    if ($col > 0) {
      if ($world->{grid}[$r][$col - 1] eq ' ') {
        $world->{grid}[$r][$col - 1] = '.';
      } elsif ($world->{grid}[$r][$col - 1] ne '.') {
        goto iv_fail;
      }
    }
    if ($col < $WIDTH - 1) {
      if ($world->{grid}[$r][$col + 1] eq ' ') {
        $world->{grid}[$r][$col + 1] = '.';
      } elsif ($world->{grid}[$r][$col + 1] ne '.') {
        goto iv_fail;
      }
    }
  }
  if (0 != sum values $world->{remain}{$city}->%*) {
  iv_fail:
    restore;
    if (@index_backup) {
      goto iv_bad_choice;
    }
    return 0;
  } else {
    # Don't disguard because the next thing that it would have to do is backup on the return side.
    # discard;
  }
  # TODO: Not really much I can do about this greediness...
  while(@index_backup) {
    pop @index_backup;
    discard;
  }
  say ' 'x($depth-1) . "IV: $city [$row, $col] SUCCESS";
  print_grid();
  return 1;
}


sub insert_horizontal($city, $row, $col) {
  return 0 if $world->{used}{$city};
  return 0 if $col < 0;
  return 0 if $col + length $city > $WIDTH;
  my @letters = split(//, $city);
  my @changed;
  backup;
  if ($col > 0) {
    if ($world->{grid}[$row][$col - 1] eq ' ') {
      $world->{grid}[$row][$col - 1] = '.';
    } elsif ($world->{grid}[$row][$col - 1] ne '.') {
      restore;
      return 0;
    }
  }
  if ($col + length $city < $WIDTH) {
    if ($world->{grid}[$row][$col+length $city] eq ' ') {
      $world->{grid}[$row][$col+length $city] = '.';
    } elsif ($world->{grid}[$row][$col+length $city] ne '.') {
      restore;
      return 0;
    }
  }
  $world->{used}{$city} = 1;
  say ' 'x$depth . "IH: $city [$row, $col]";
  local $depth = $depth + 1;
  for my $i (keys @letters) {
    if ($world->{grid}[$row][$col+$i] eq ' ') {
      $world->{grid}[$row][$col+$i] = $letters[$i];
      $world->{pick}[$row][$col+$i] = $city;
      push(@changed, [$letters[$i], $col+$i]);
    } elsif($world->{grid}[$row][$col+$i] eq $letters[$i]) {
      my $cross = $world->{pick}[$row][$col+$i];
      confess unless $cross;
      if ($world->{remain}{$city}{$cities{$cross}{country}} && $world->{remain}{$cross}{$cities{$city}{country}}) {
        --$world->{remain}{$city}{$cities{$cross}{country}};
        --$world->{remain}{$cross}{$cities{$city}{country}};
      } else {
        restore;
        return 0;
      }
    } else {
      restore;
      return 0;
    }
  }
  my ($changed_i, $country_i, $city_i);
  my @countries = sort keys $world->{remain}{$city}->%*;
  my @index_backup;
 ih_changed:
  for ($changed_i = 0; $changed_i < @changed; ++$changed_i) {
    my ($letter, $c) = $changed[$changed_i]->@*;
    for ($country_i = 0; $country_i < @countries; ++$country_i) {
      my @cities = $countries{$countries[$country_i]}->@*;
      next unless $world->{remain}{$city}{$countries[$country_i]};
      for ($city_i = 0; $city_i < @cities; ++$city_i) {
        my $citi = $cities[$city_i];
        next if $world->{used}{$citi};
        next unless $world->{remain}{$citi}{$cities{$city}{country}};
        my $last_index = 0;
        my $index;
        while(($index = index($citi, $letter, $last_index)) != -1) {
          if (insert_vertical($citi, $row - $index, $c)) {
            push(@index_backup, [$changed_i, $country_i, $city_i, $index]);
            next ih_changed;
          ih_bad_choice:
            ($changed_i, $country_i, $city_i, $index) = shift(@index_backup)->@*;
            ($letter, $c) = $changed[$changed_i]->@*;
            @cities = $countries{$countries[$country_i]}->@*;
            $citi = $cities[$city_i];
          }
          $last_index = $index + 1;
        }
      }
    }
    if ($row > 0) {
      if ($world->{grid}[$row - 1][$c] eq ' ') {
        $world->{grid}[$row - 1][$c] = '.';
      } elsif ($world->{grid}[$row - 1][$c] ne '.') {
        goto ih_fail;
      }
    }
    if ($row < $HEIGHT - 1) {
      if ($world->{grid}[$row + 1][$c] eq ' ') {
        $world->{grid}[$row + 1][$c] = '.';
      } elsif ($world->{grid}[$row + 1][$c] ne '.') {
        goto ih_fail;
      }
    }
  }
  if (0 != sum values $world->{remain}{$city}->%*) {
  ih_fail:
    restore;
    if (@index_backup) {
      goto ih_bad_choice;
    }
    return 0;
  } else {
    # Don't disguard because the next thing that it would have to do is backup on the return side.
    # discard;
  }
  # TODO: Not really much I can do about this greediness...
  while(@index_backup) {
    pop @index_backup;
    discard;
  }
  say ' 'x($depth-1) . "IH: $city [$row, $col] SUCCESS";
  print_grid();
  return 1;
}


if(!insert_vertical('VILNIUS', 0, 16)) {
  say "Insert failed :(";
} else {
  p($world->{remain});
}

=pod

my $target = uc $ARGV[0];

for my $country ($cities{$target}->{intersects}->@*) {
  my @cities = $countries{$country}->@*;
  my @matches;
  for my $letter (split(//, $target)) {
    for my $city (@cities) {
      my $index = index($city, $letter);
      if (($index != -1)) {
        my $dup = $city;
        substr($dup, $index, 1) = "[$letter]";
        push(@matches, $dup);
      }
    }
  }
  say "$country: " . join(' ', @matches);
}

=cut

__DATA__
ANTWERP	Belgium	France	Germany	Hungary	Netherlands
BARCELONA	Spain	France	Latvia	Poland	Romania
BARI	Italy	Bulgaria
BRATISLAVA	Slovakia	Bulgaria	Germany	Italy	Italy
BUCHAREST	Romania	Poland	Portugal	Spain
BUDAPEST	Hungary	Belgium	France
CARDIFF	Wales	France	Germany	Italy
COGNAC	France	Spain	Wales
COLOGNE	Germany	Poland	Poland
COPENHAGEN	Denmark	France	Germany	Italy	Poland	Sweden
DUSSELDORF	Germany	Norway	Poland
FLORENCE	Italy	Germany	Netherlands	Poland	Wales
FRANKFURT	Germany	Belgium	Italy
GDANSK	Poland	Germany	Germany	Poland
HALLE	Germany	Denmark	Italy
HAMBURG	Germany	Sweden
HANAU	Germany	Germany	Slovakia
HANOVER	Germany	Finland	Italy
HELSINKI	Finland	Germany	Italy	Spain
INNSBRUCK	Austria	France
KATOWICE	Poland	Bulgaria	Denmark	Germany	Italy
KRAKOW	Poland	Norway	Poland
LASPALMAS	Spain	Finland	Italy	Portugal
LEIPZIG	Germany	Italy	Lithuania
LISBON	Portugal	France	Poland	Romania	Spain
LODZ	Poland	France
LUBLIN	Poland	Portugal	Spain
LYON	France	Poland	Portugal
MILAN	Italy	Finland	Germany	Slovakia
MUNICH	Germany	Germany	Italy
NANTES	France	Austria	Belgium	Denmark	France
NAPLES	Italy	Bulgaria	Denmark
OSLO	Norway	Germany	Poland
PALERMO	Italy	Czech Republic	France	Germany	Slovakia	Sweden
PARIS	France	Italy	Lithuania
PILSEN	Czech Republic	Italy	Lithuania
PLOVDIV	Bulgaria	Italy	Poland
POZZUOLI	Italy	Germany	Sweden
RIGA	Latvia	Austria	Spain
ROME	Italy	Germany	Spain
SEVILLE	Spain	Lithuania
SOFIA	Bulgaria	Italy	Slovakia
STOCKHOLM	Sweden	Denmark	Germany	Italy	Italy
THEHAGUE	Netherlands	Belgium	Italy
TOULOUSE	France	France	Hungary
VIENNA	Austria	Latvia
VILNIUS	Lithuania	Czech Republic	France	Germany	Spain
WARSAW	Poland	Poland	Romania
WEIMAR	Germany	Poland	Wales
WROCLAW	Poland	Germany	Poland
