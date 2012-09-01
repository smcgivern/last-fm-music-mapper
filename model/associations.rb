class UserArtist
  include DataMapper::Resource

  belongs_to :user, :key => true
  belongs_to :artist, :key => true
  belongs_to :period, :key => true

  property :play_count, Numeric
end

class TagCountry
  include DataMapper::Resource

  belongs_to :tag, :key => true
  belongs_to :country, :key => true
end
