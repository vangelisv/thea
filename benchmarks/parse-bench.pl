#!/usr/bin/perl -w

use strict;

while (my $f = shift @ARGV) {
    if ($f =~ /bench\d\-(\S+).out/) {
        parse_bench($f,$1);
    }
    elsif ($f =~ /owlapi\d\-(\S+).out/) {
        parse_owlapi($f,$1);
    }
    else {
        die $f;
    }
}
exit 0;

sub parse_bench {
    my $f = shift;
    my $id = shift;
    open(F,$f) || die $f;
    while (<F>) {
        if (/load_time:\s+(\S+)/) {
            print "TIME\tTHEA\t$id\t$1\n";
        }
        if (/^Heap\s+:\s+(\d+),(\d+),(\d+) Bytes/) {
            my $m = $1*1000000 + $2*1000 + $3;
            $m /= 1000000;
            print "MEM\tTHEA\t$id\t$m\n";
        }
    }
    close(F);
}

sub parse_owlapi {
    my $f = shift;
    my $id = shift;
    my $n=0;
    open(F,$f);
    while (<F>) {
        if (/^real\s+(\d+)m(\S+)s/) {
            my $s = $1 * 60 + $2;
            print "TIME\tOWLAPI\t$id\t$s\n";
        }
        if (/diff:(\d+)\s+\(bytes/) {
            $n++;
            my $m = $1/1000000;
            print "MEM\tOWLAPI\t$id\t$m\n"
                if $n == 2;
        }
    }
    close(F);
}
