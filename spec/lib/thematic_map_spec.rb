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

describe 'ThematicMap#value_division' do
  it 'should place the value in a division, by index' do
    ThematicMap.new do |map|
      map.division_method = :equal_count_division
      map.value_division(1, [1, 2, 3]).should.equal 0
      map.value_division(1, [0, 1, 2]).should.equal 1
      map.value_division(1, [-1, 0, 1]).should.equal 2
    end
  end

  it 'should use the highest possible division' do
    ThematicMap.new do |map|
      map.division_method = :equal_count_division
      map.value_division(1.5, [1, 2, 3]).should.equal 0
      map.value_division(1.5, [0, 1, 2]).should.equal 1
      map.value_division(1.5, [-1, 0, 1]).should.equal 2
    end
  end

  it 'should use @division_method' do
    values = [1, 2, 3, 10]

    ThematicMap.new do |map|
      map.division_method = :equal_count_division
      map.value_division(3, values).should.equal 2
      map.division_method = :equal_ranges_division
      map.value_division(3, values).should.equal 0
    end
  end
end

describe 'ThematicMap#equal_count_division' do
  it 'should put an equal number of items in division' do
    ThematicMap.new.equal_count_division([1, 2, 3, 4, 5, 100]).
      should.equal [1, 3, 5]
  end

  it 'should not duplicate range beginnings' do
    ThematicMap.new.equal_count_division([1, 1, 2, 3, 3]).
      should.equal [1, 2, 3]
  end
end

describe 'ThematicMap#equal_ranges_division' do
  it 'should make each division the same size' do
    ThematicMap.new.equal_ranges_division([1, 2, 3, 4, 5, 10]).
      should.equal [1, 4, 7]
  end

  it 'should not require a value in each division' do
    ThematicMap.new.equal_ranges_division([1, 2, 3, 4, 5, 100]).
      should.equal [1, 34, 67]
  end
end
