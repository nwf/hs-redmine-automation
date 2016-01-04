#!/usr/bin/env perl

use strict;
use JSON;
use Data::Dumper;

my $BOX_DEVELOPER_KEY = "XXX"; # put dev key here

my $json = from_json(<STDIN>);

my $entries = $$json{'item_collection'}{'entries'};

foreach my $entry (@$entries) {
  $$entry{'name'} =~ /.*\((\d+)\)$/ or die;
  my $clientid = $1;
  my $boxid = $$entry{'id'};

  system "curl https://api.box.com/2.0/folders/$boxid -H 'Authorization: Bearer $BOX_DEVELOPER_KEY' > box/folder-$boxid-for-$clientid.json";

  sleep 5;
}
