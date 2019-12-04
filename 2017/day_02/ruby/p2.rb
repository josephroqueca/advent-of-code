#!/usr/bin/env ruby
# frozen_string_literal: true

script_dir = __dir__
filename = format('%<script_dir>s/../input.txt', script_dir: script_dir)
input = File.readlines(filename)

checksum = 0

input.each do |line|
  value_found = false

  line.split("\t").each do |a|
    next if value_found

    a_int = a.to_i
    line.split("\t").each do |b|
      next if value_found

      b_int = b.to_i
      if a_int > b_int && (a_int % b_int).zero?
        checksum += a_int / b_int
        value_found = true
      elsif a_int < b_int && (b_int % a_int).zero?
        checksum += b_int / a_int
        value_found = true
      end
    end
  end
end

puts format("The sum of each row's result is %<checksum>d", checksum: checksum)
