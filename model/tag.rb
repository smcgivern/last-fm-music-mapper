class Tag
  include DataMapper::Resource

  has n, :artists, :through => Resource
  has n, :tag_countries

  property :tag, String, :key => true
end
