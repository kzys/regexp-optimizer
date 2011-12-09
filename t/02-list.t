#
# $Id: 02-list.t,v 0.4 2004/11/05 12:44:48 dankogai Exp $
#
use strict;
use warnings;
use Test::More tests => 24;
use Regexp::Optimizer;
my $l = Regexp::List->new->new(lookahead => 0);
my $o = Regexp::Optimizer->new;
my $Debug = shift;
# classical
my %t_l =
    (
     qq/foobar fooxar/  => qr/foo[bx]ar/,
     qq/not optimized/  => qr/(?:not|optimized)/,
     qq/1 2/            => qr/[12]/,
     qq/1 12/           => qr/12?/,
     qq/1 12 123/       => qr/1(?:23?)?/,
     qq/aa ab/          => qr/a[ab]/,
    );

# Trivial ones

for (sort {length $a <=> length $b} keys %t_l){
    no warnings 'uninitialized';
    my @words = split /\s+/, $_;
    my $regex = join '|' => @words;
    is($l->list2regex(@words) => $t_l{$_}, qq/l->l($_) eq $t_l{$_}/);
    is($o->optimize($regex)   => $t_l{$_}, qq/o->o($regex) eq $t_l{$_}/);
}

# Slightly less trivial

%t_l = 
    (
     q/\012|\015/     => qr/\\01[25]/,
     q/\x20|\x3F/ =>  => qr/\\x(?:20|3F)/,
     q/\cZ|\cA/       => qr/\\c[ZA]/,
);

# perldelta 5.14
# Accept both old and new-style stringification
my $modifiers = (qr/foobar/ =~ /\Q(?^/) ? "^" : "-xism";

my %t_o = 
    (
     q/\012|\015/     => qr/[\012\015]/,
     q/\x20|\x3F/ =>  => qr/[\x20\x3F]/,
     q/\cZ|\cA/       => $] < 5.008005 ? qr/[\cZ\cA]/ : "(?$modifiers:[\\cZ\\cA])",
    );

for (sort {length $a <=> length $b} keys %t_l){
    no warnings qw/regexp uninitialized/;
    my @words = split /\|/, $_;
    my $regex = $_;
    is($l->list2regex(@words) => $t_l{$_}, qq/l->l($_) eq $t_l{$_}/);
    isnt($o->optimize($regex) => $t_l{$_}, qq/o->o($regex) ne $t_l{$_}/);
    is($o->optimize($regex) => $t_o{$_}, qq/o->o($regex) eq $t_o{$_}/);
}

my @words;
open F, $0 or die "$0:$!";
while(<F>){
    push @words, $1 while( /([A-Za-z]+)/gc );
}

my @match;
my $re_l = $l->list2regex(@words);
$Debug and warn $re_l;
open F, $0 or die "$0:$!";
while(<F>){
    push @match, $1 while( /($re_l)/gc );
}
close $0;
ok(eq_array(\@words, \@match), "l->l(): all words in $0");

@match = ();
my $re_trivial = join('|' => map {quotemeta($_) } @words);
my $re_o = $o->optimize($re_trivial);
$Debug and warn $re_o;
open F, $0 or die "$0:$!";
while(<F>){
     push @match, $1 while( /($re_o)/gc );
}
close $0;
ok(eq_array(\@words, \@match), "o->o(): all words in $0");
is($re_l, $re_o, "l->l and o->o agrees");
__END__
