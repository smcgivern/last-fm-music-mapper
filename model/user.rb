class User
  include DataMapper::Resource

  has n, :user_artists

  property :username, String, :key => true
  property :updated_at, Time

  def load_artists(user_artists)
    return if updated_at < (Time.now - 7 * 24 * 60 * 60)

    period = user_artists['topartists']['@attr']['type']

    user_artists['topartists']['artist'].each do |a|
      artist = Artist.first_or_create(:music_brainz_id => Artist.make_mbid(a))

      next if artist.updated_at < (Time.now - 7 * 24 * 60 * 60)

      image = a['image'].select {|x| x['size'] == 'medium'}.first['#text']

      # May as well keep these current every time.
      artist.update(:name => a['name'], :url => a['url'], :image => image)

      UserArtist.first_or_create(:user => self,
                                 :artist => artist,
                                 :period => Period(period),
                                 :play_count => a['playcount'])
    end

    reload
  end
end
