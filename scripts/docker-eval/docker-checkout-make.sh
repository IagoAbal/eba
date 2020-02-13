echo "Changing directory.."
cd linux
echo "Removing .git/index.lock.."
rm .git/index.lock
echo "Checking out $HASH.."
git checkout --force $HASH
echo "Executing 'make allyesconfig'.."
make allyesconfig
echo "Executing 'make $FILE'.."
make $FILE
echo "Removing output not supported by EBA.."
sed -i "/^_Static_assert/ d" $FILE
sed -i "s/asm __inline volatile/asm volatile/g" $FILE