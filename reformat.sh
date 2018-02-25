COMMON_ANCESTOR=`git merge-base upstream/refactoring HEAD`

git diff -U0 $COMMON_ANCESTOR | ./clang-format-diff.py -i -p1
find lib/Crellvm -name "*.cpp" | xargs clang-format -i
find include/llvm/Crellvm -name "*.h" | xargs clang-format -i
