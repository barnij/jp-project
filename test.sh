for i in {1..10};
do
    echo "test $i:"
    if [ "$1" = "-v" ]; then
        ./main testy/$i.in
    else
        ./main testy/$i.in > /dev/null
    fi
    diff testy/$i.out result.txt || exit 1
    echo "OK :)"
    echo ""
done
for i in {11..16};
do
    echo "test $i:"
    if [ "$1" = "-v" ]; then
        ./main testy/${i}a.in testy/${i}b.in
    else
        ./main testy/${i}a.in testy/${i}b.in > /dev/null
    fi
    diff testy/$i.out result.txt || exit 1
    echo "OK :)"
    echo ""
done