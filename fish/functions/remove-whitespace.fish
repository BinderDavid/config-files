# Remove trailing whitespace in a file
function remove-whitespace
    sed -i 's/[ \t]*$//' $argv
end
