class Country
  include DataMapper::Resource

  has n, :tag_countries

  property :iso_code, String, :key => true
  property :name, String
end

def Country(key); Country.first_or_create(:iso_code => key); end
