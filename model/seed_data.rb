require 'model/period'
require 'model/country'

PERIODS = [
           {:identifier => '7day', :name => 'Last 7 days'},
           {:identifier => '1month', :name => 'Last month'},
           {:identifier => '3month', :name => 'Last 3 months'},
           {:identifier => '6month', :name => 'Last 6 months'},
           {:identifier => '12month', :name => 'Last 12 months'},
           {:identifier => 'overall', :name => 'Overall'},
          ]

PERIODS.each {|d| Period(d[:identifier]).update(:name => d[:name])}

# Countries
# https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
# Add length limit to the Country model
# Find out the best way to map in D3

# Add tag countries - are tags case-sensitive?
# Rethink file layout
