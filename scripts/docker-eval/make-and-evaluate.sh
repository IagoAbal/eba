echo "Running docker container.." 
docker run  -v "$(pwd)/$1":"/linux/$(dirname "$2")" --env "HASH=$1" --env "FILE=$2" ubuntu-linux

echo "Cleaning up.."
output_file=$(basename $2)
original_file=${output_file%.i}.c
cp $1/$output_file $output_file
cp $1/$original_file $original_file
rm -r $1 
mkdir $1
mv $output_file $1/$output_file
mv $original_file $1/$original_file

echo "Running EBA.."
./../../bin/eba --loop-limit=1 --branch-limit=1 --all-lock-types --dUa "$1/$(basename "$2")" | tee "$1/$1.txt"
echo "Done. Results have been written to $1/$1.txt."