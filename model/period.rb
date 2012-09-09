class Period
  include DataMapper::Resource

  property :identifier, String, :key => true
  property :name, String
end

def Period(key)
  return key if key.instance_of?(Period)

  Period.first_or_create(:identifier => key)
end
