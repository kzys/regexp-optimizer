use strict;
use warnings;
use Test::More tests => 1;
use Regexp::Optimizer;

my @headers = qw(
    ACCEPT-ENCODING
    AUTHORIZATION
    CACHE-CONTROL
    CONTENT-TYPE
    USER-AGENT
    X-FORWARDED-FOR
    X-JPHONE-.*
    X-S-DISPLAY-INFO
    X-UP-.*
);

my $pattern = '^(' . join("|", @headers) . ')$';
my $optimizer = Regexp::Optimizer->new;
my $options = $^V gt v5.14.0 ? '^i' : 'i-xsm';
is(
    $optimizer->optimize(qr/$pattern/i),
    qr/(?$options:^(?:A(?:CCEPT-ENCODING|UTHORIZATION)|C(?:ACHE-CONTROL|ONTENT-TYPE)|X-(?:FORWARDED-FOR|(?:JPHONE|UP)-.*|S-DISPLAY-INFO)|USER-AGENT)$)/
);
