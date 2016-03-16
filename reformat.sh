COMMON_ANCESTOR=`git merge-base upstream/release_37_vellvm HEAD`

git diff -U0 $COMMON_ANCESTOR | ./clang-format-diff.py -i -p1
find lib/LLVMBerry -name "*.cpp" | xargs clang-format -i
find include/llvm/LLVMBerry -name "*.h" | xargs clang-format -i
