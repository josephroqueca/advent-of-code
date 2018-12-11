#!/usr/bin/env ruby

script_dir = File.expand_path(File.dirname(__FILE__))
filename = '%s/../input.txt' % script_dir
input = File.readlines(filename)
input = input[0]

sum = 0
input.split("").each_with_index do |c, i|
    if (i < input.length - 1 && c == input[i + 1]) || (i == input.length - 1 && c == input[0]) then
        sum += c.to_i
    end
end

puts sum