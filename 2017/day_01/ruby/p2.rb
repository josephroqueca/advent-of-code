#!/usr/bin/env ruby

input = File.readlines('../input.txt')
input = input[0]

sum = 0
input.split("").each_with_index do |c, i|
    if c == input[(i + input.length / 2) % input.length] then
        sum += c.to_i
    end
end

puts sum