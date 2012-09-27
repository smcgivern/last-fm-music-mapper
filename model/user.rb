class User
  include DataMapper::Resource

  has n, :user_artists

  property :username, String, :key => true
  property :updated_at, Time

  def load_artists(api_response)
    period = Period(api_response['topartists']['@attr']['type'])

    if (user_artists(period).length > 0 and
        updated_at >= (Time.now - 7 * 24 * 60 * 60))
      return self
    end

    api_response['topartists']['artist'].each do |a|
      artist = Artist.first_or_create(:music_brainz_id => Artist.make_mbid(a))

      # May as well keep these current every time.
      artist.name = a['name']
      artist.url = a['url']
      artist.image = a['image'].select {|x| x['size'] == 'medium'}[0]['#text']
      artist.save

      user_artist = UserArtist.first_or_create(:user => self,
                                               :artist => artist,
                                               :period => period)

      user_artist.play_count = a['playcount']
      user_artist.save
    end

    self
  end

  def user_artists(period='overall')
    super(:period => Period(period))
  end
end
