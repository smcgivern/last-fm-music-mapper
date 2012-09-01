class Period
  include DataMapper::Resource

  property :identifier, String, :key => true
  property :name, String
end

def Period(key); Period.first_or_create(:identifier => key); end
