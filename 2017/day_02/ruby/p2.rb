#!/usr/bin/env ruby

script_dir = File.expand_path(File.dirname(__FILE__))
filename = '%s/../input.txt' % script_dir
input = File.readlines(filename)

checksum = 0

input.each do |line|
    value_found = false

    line.split("\t").each do |a|
        if value_found then next end

        a_int = a.to_i
        line.split("\t").each do |b|
            if value_found then next end

            b_int = b.to_i
            if a_int > b_int && a_int % b_int == 0 then
                checksum += a_int / b_int
                value_found = true
            elsif a_int < b_int && b_int % a_int == 0 then
                checksum += b_int / a_int
                value_found = true
            end
        end
    end
end

puts checksum