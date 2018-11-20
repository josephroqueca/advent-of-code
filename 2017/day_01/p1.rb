input = nil

if File.basename(Dir.getwd).match?(/day\d\d$/) then
    input = File.readlines('input.txt')
else
    input = File.readlines('day01/input.txt')
end

input = input[0]

sum = 0
input.split("").each_with_index do |c, i|
    if (i < input.length - 1 && c == input[i + 1]) || (i == input.length - 1 && c == input[0]) then
        sum += c.to_i
    end
end

puts sum