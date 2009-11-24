#!/usr/bin/perl -w

use FileHandle;

my $fname = "testdata/mytest.dat";
#my @companies = ("IBM", "Oracle", "Sun", "Google");
#my @companies = ("IBM", "Oracle", "Sun", "Google", "Oracle", "Oracle", "Oracle", "Sun", "Sun", "Sun", "Sun");
#my @companies = ("IBM", "Sun", "Oracle", "Oracle", "Oracle", "Oracle");
my @companies = ("IBM", "Sun", "Oracle");
my @prices = (65.32);
my @prices_Sun = (32.56);
my @prices_IBM = (54.32, 12.99);
my @prices_Google = (32.56);
my @prices_Yahoo = (60, 60, 60, 20);

sub generate_random_name {
  my @names = @_;
  my $random_name = $names[rand @names];
  return $random_name;
}

sub generate_random_price {
  my @prices = @_;
  my $random_price = $prices[rand @prices];
  return $random_price;
}

sub create_data {
  my $rows = shift;
  my $fh = new FileHandle(">$fname") or die "Couldn't create $fname: $!\n";

  for(my $i=0; $i<$rows; $i++) {
    my $record =  "". $i. ",". &generate_random_name(@companies). ",". 
	&generate_random_price(@prices);
    print $fh "$record\n";    
  }
  $fh->close();
}

sub create_data_selectivity {
  my $rows = shift;
  my $fh = new FileHandle(">$fname") or die "Couldn't create $fname: $!\n";

  for(my $i=0; $i<$rows; $i++) {
    my $name = &generate_random_name(@companies);

    my $record =  "". $i. ",". $name. ","; 
    my $price = "";
    if($name eq "IBM") { $price = &generate_random_price(@prices_IBM); }
    elsif($name eq "Sun") { $price = &generate_random_price(@prices_Sun); }
    else { $price = &generate_random_price(@prices); }

    $record = $record. $price;    
    print $fh "$record\n";    
  }
  $fh->close();
}

#&create_data(6000);
&create_data_selectivity(10000);
#&create_data_selectivity(10);



