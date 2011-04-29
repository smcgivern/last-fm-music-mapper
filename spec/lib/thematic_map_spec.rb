require 'spec/setup'
require 'lib/thematic_map'

describe 'ThematicMap.new' do
  it 'should provide the object as a block' do
    ThematicMap.new {|m| m.map = :foo}.map.should.equal :foo
  end
end

describe 'ThematicMap#map_file' do
  it 'should return the map file given in @map, if a string' do
    ThematicMap.new {|m| m.map = '~/europe_map.xml'}.map_file.
      should.equal '~/europe_map.xml'
  end

  it 'should pick a SVG file from lib/thematic_map/maps/, if a symbol' do
    ThematicMap.new {|m| m.map = :asia_map}.map_file.
      should.equal './lib/thematic_map/maps/asia_map.svg'
  end
end

describe 'ThematicMap#map_xml' do
  it 'should be a Nokogiri XML document of #map_xml' do
    ThematicMap.new {|m| m.map = 'spec/fixture/thematic_map.xml'}.map_xml.
      should.be.a lambda {|x| Nokogiri::XML::Document === x}
  end
end

describe 'ThematicMap#number_of_divisions' do
  it 'should use the @division_size attribute' do
    thematic_map = ThematicMap.new do |map|
      map.colour_palette = [[:a], [:a, :b], [:a, :b, :c], [:a, :b, :c, :d]]
    end

    thematic_map.division_size = 4
    thematic_map.number_of_divisions([1, 2, 3, 4]).should.equal 1

    thematic_map.division_size = 2
    thematic_map.number_of_divisions([1, 2, 3, 4]).should.equal 2

    thematic_map.division_size = 1
    thematic_map.number_of_divisions([1, 2, 3, 4]).should.equal 4
  end

  it 'should pick the shortest palette if there are too few divisions' do
    ThematicMap.new do |map|
      map.division_size = 5
      map.colour_palette = [[:red, :blue, :green], [:red, :blue]]
    end.number_of_divisions([1, 2, 3]).should.equal 2
  end

  it 'should pick the longest palette if there are too many divisions' do
    ThematicMap.new do |map|
      map.division_size = 1
      map.colour_palette = [[:red], [:red, :blue]]
    end.number_of_divisions([1, 2, 3]).should.equal 2
  end
end
