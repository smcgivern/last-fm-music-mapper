Small [Sinatra](http://sinatrarb.com/) app that makes music maps from
[Last.fm](http://last.fm/) profiles.

The artist countries are determined by the tags assigned to each artist. This
means that many are missing or wrong (for instance, Anaïs Mitchell is from
Vermont, but has tags for 'united kingdom'). Still, that's the cost of using
free data.

The API client for Last.fm is at `lib/last_fm.rb` while `lib/music_mapper.rb`
does the bulk of the work in gluing everything together.
