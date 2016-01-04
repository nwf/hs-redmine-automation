#!/usr/bin/env perl

# Could stand to have "warnings" fleshed out a bit and pushed into the
# various extractors, so that we could do things like "No institutions"
# or so on.

use strict;
use warnings;
use English;

use Text::CSV;
use JSON;

use Data::Dumper;

######################## COLUMN HEADING VALUES ############################

my $ch_firstname = 'First Name';
my $ch_lastname = 'Last Name';
my $ch_clientid = "Applicant Client ID";

my $ch_area  = "Area of Interest";
my $ch_area2 = "Secondary Area of Interest"; # optional

my $ch_triageA = "Triage A";
my $ch_triageAEmail = "Triage A Email";
my $ch_triageB = "Triage B";
my $ch_triageBEmail = "Triage B Email";
my $ch_foi = "Faculty of Interest";

my $ch_citizen = "Citizenship";

my $ch_gre_summary = "GRE Scores";
my $ch_toefl_summary = "TOEFL Score";
my $ch_inst_summary = "Prior Institutions";

my $ch_gre_qs = "GRE Quant Score";
my $ch_gre_qp = "GRE Quant Percent";
my $ch_gre_vs = "GRE Verb score";
my $ch_gre_vp = "GRE Verbal Percent";
my $ch_gre_as = "AWA_SCORE";
my $ch_gre_ap = "Analytical Writing Percent";
my @ch_gre_sfx = ("", " 2");

my $ch_inst_inst = "Inst ";
my $ch_inst_deg = "Deg ";
my $ch_inst_gpa = "GPA";
my @ch_inst_sfx = ("1", "2", "3");


sub checkStr($) {
  my ($str) = @_;
  return (defined $str and $str ne "");
}

############################### INIT ######################################


sub checkCommonFields($) {
  my ($hash) = @_;

  foreach my $field
    ($ch_triageA, $ch_triageB, $ch_firstname, $ch_lastname, $ch_citizen, $ch_area) {

    die "Missing field '$field'" unless checkStr($$hash{$field});
  }
}

sub makeName($) {
  my ($hash) = @_;
  return "$$hash{$ch_lastname}, $$hash{$ch_firstname}";
}

my @printFields = ($ch_triageA, $ch_triageB, $ch_gre_summary, $ch_toefl_summary, $ch_inst_summary);

sub printFields($$) {
  my ($fh, $hash) = @_;

  my $subject = makeName($hash);
  my @warnings;
 
  print $fh "From: Redmine <redmine-admin\@clsp-redmine.cs.jhu.edu>\n";
  print $fh "To: Testing <testing\@clsp-redmine.cs.jhu.edu>\n";
  print $fh "Cc: $$hash{$ch_triageAEmail}, $$hash{$ch_triageBEmail}\n";
  print $fh "Date: " . `date`;
  print $fh "Subject: $subject\n";
  print $fh "\n";
  print $fh "Status:\tIn Triage\n";
  ### Prefer watchers set via Cc: above.
  # print $fh "Assignee:\t$$hash{$ch_triageA}\n";
  foreach my $field (@printFields) {
    print $fh "$field:\t$$hash{$field}\n" if checkStr($$hash{$field});
  }

  if (checkStr($$hash{$ch_area})) {
    if (checkStr($$hash{$ch_area2})) {
      print $fh "Research Areas:\t$$hash{$ch_area}; $$hash{$ch_area2}\n";
    } else {
      print $fh "Research Areas:\t$$hash{$ch_area}\n";
    }
  } else { die if ($$hash{$ch_area2} ne ""); }

  if (checkStr($$hash{$ch_foi})) {
    print $fh "Faculty of Interest:\t$$hash{$ch_foi}\n";
  }

  if ($$hash{$ch_citizen} eq "U.S. Citizen") {
    print $fh "US Citizen:\tYes\n";
  } else {
    print $fh "US Citizen:\tNo\n";
  }

  my $box_folder_json_file = (glob "box/*$$hash{$ch_clientid}.json")[0];
  if (not defined $box_folder_json_file) {
    push @warnings, "Unable to find applicant ($$hash{$ch_clientid}) in Box list; no link to application will be generated";
  } else {
    open my $box_folder_json, "<", $box_folder_json_file
     or die "Unable to open box folder json";
    my $box_folder = from_json(<$box_folder_json>);
    close $box_folder_json;

    my $box_folder_items = $$box_folder{'item_collection'}{'entries'};
    my $box_combined_pdf_id = undef;
    foreach my $entry (@$box_folder_items) {
      if ($$entry{'name'} =~ /Combined/) {
        $box_combined_pdf_id = $$entry{'id'};
        last;
      }
    }
    my $pdf_link = undef;
    if(defined $box_combined_pdf_id) {
      $pdf_link = "https://jh.app.box.com/files/0/f/$$box_folder{'id'}/1/f_$box_combined_pdf_id"
    } else {
      push @warnings, "No combined PDF found; linking to folder instead.\n";
      $pdf_link = "https://jh.app.box.com/files/0/f/$$box_folder{'id'}/"
    }
    
    print $fh "PDF Application:\t$pdf_link\n";
  }

  print $fh "\n\n";

  if (scalar @warnings) {
    print $fh "There were warnings generated during automatic processing:\n";
    foreach my $warning (@warnings) {
      print $fh " - $warning\n";
    }
  }
}

############################### GRE #######################################


# e.g. "800M 760V (99%, 94%); 760M 800V (96%, 98%)"
# If we don't have %iles, use "??"
sub makeGRE($) {
  my ($hash) = @_;
  my @scores = ();

  foreach my $sfx (@ch_gre_sfx) {
    my $res = "";
    my @pcts = ();

    next unless (checkStr($$hash{$ch_gre_qs.$sfx}))
             or (checkStr($$hash{$ch_gre_vs.$sfx}))
             or (checkStr($$hash{$ch_gre_as.$sfx}));

    if (checkStr($$hash{$ch_gre_qs.$sfx})) { 
      $res .= "$$hash{$ch_gre_qs.$sfx}M";
      push @pcts, ($$hash{$ch_gre_qp.$sfx} or "??");
    }

    if (checkStr($$hash{$ch_gre_vs.$sfx})) {
      $res .= " $$hash{$ch_gre_vs.$sfx}V";
      push @pcts, ($$hash{$ch_gre_vp.$sfx} or "??");
    }

    if (checkStr($$hash{$ch_gre_as.$sfx})) {
      $res .= " $$hash{$ch_gre_as.$sfx}AW";
      push @pcts, ($$hash{$ch_gre_ap.$sfx} or "??");
    }

    $res .= " (" . (join ",", (map { $_ . "%" } (@pcts))) . ")";
    push @scores, $res;
  }

  return (join "; ", @scores);
}

############################# TOEFL #######################################

# XXX Should use column heading names

sub makeTOEFL($) {
  my ($hash) = @_;

  return undef if not checkStr($$hash{"TOEFL_TOTAL_SCORE"});

  my $sfx = "";
  if (checkStr($$hash{'TOEFL_CBT_PBT'})) {
    $$hash{'TOEFL_CBT_PBT'} =~ /^[^(]*(\([^)]+\))$/;
    $sfx = (((defined $1) and " $1") or $$hash{'TOEFL_CBT_PBT'});
  }

  return ("$$hash{'TOEFL_TOTAL_SCORE'}:"
        . " " . ($$hash{'TOEFL_LISTENING_SCORE'} or "?")      . "(Listening),"
        . " " . ($$hash{'TOEFL_READING_SCORE'} or "?")        . "(Reading),"
        . " " . ($$hash{'TOEFL_SPEAKING_SCORE'} or "?")       . "(Speaking),"
        . " " . ($$hash{'TOEFL_STRUCTURE_WRTG_SCORE'} or "?") . "(Writing)"
        . $sfx);
}

############################# INSTS #######################################

sub makeInst($) {
  my ($hash) = @_;
  my @insts = ();
 
  foreach my $sfx (@ch_inst_sfx) {
    next unless (checkStr($$hash{$ch_inst_inst.$sfx}))
             or (checkStr($$hash{$ch_inst_deg.$sfx}))
             or (checkStr($$hash{$ch_inst_gpa.$sfx}));

    push @insts, ("$$hash{$ch_inst_inst.$sfx} "
                  . "($$hash{$ch_inst_deg.$sfx}: $$hash{$ch_inst_gpa.$sfx})");
  } 

  return (join "; ", @insts);
}

############################# LOOP ########################################

my $csv = Text::CSV->new ( { binary => 1 } );

$csv->column_names($csv->getline(*STDIN)) or die;
my @colheads = $csv->fields();

# print "There are $#colheads columns in this file.\n";

while(my $valhash = $csv->getline_hr(*STDIN)) {
  checkCommonFields($valhash);

  ## foreach my $k (sort keys %{$valhash}) {
  ##   print "\t$k => $$valhash{$k}\n";
  ## }
  ## print "\n";

  $$valhash{$ch_gre_summary} = makeGRE($valhash);
  $$valhash{$ch_toefl_summary} = makeTOEFL($valhash);
  $$valhash{$ch_inst_summary} = makeInst($valhash);

  my $fh;
  open $fh, ">", ("split/" . makeName($valhash)) or die $!;
  printFields($fh, $valhash);
  close $fh;
}
