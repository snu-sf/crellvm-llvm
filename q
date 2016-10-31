commit 34978b814b6f3f91867bbea8c48c8dedaff3309b
Merge: 5e0f841 224bf82
Author: markshin <shin.dongyeon@gmail.com>
Date:   Sat Oct 29 19:40:42 2016 +0900

    Merge branch 'refactoring_debug' of https://github.com/SanghoonPark/llvm into refactoring_foldPHIbin

commit 224bf820805c1e9007eef61051f06db16cef57e5
Merge: 6711eac 496e501
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Oct 28 11:34:45 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring_debug
    
    Conflicts:
    	include/llvm/LLVMBerry/Hintgen.h
    	lib/LLVMBerry/Hintgen.cpp
    	lib/LLVMBerry/Structure.cpp

commit 6711eac750a2d1e9b9ad8cd8120a587acae25e9f
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Oct 28 11:25:43 2016 +0900

    Fix bug in replace algorithm of hint generation code in mem2reg

commit 496e501c912ecde9cd31d61bc911ce4ebb19f12a
Merge: 3a740ad 141d103
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Wed Oct 26 18:09:52 2016 +0900

    Merge pull request #215 from aqjune/clangoptions
    
    Let llvmberry runtime options be enabled in clang as well.

commit 3a740ad9df5b928b562544d83748cdd5f7d87854
Merge: 08722e2 6f31f67
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Tue Oct 25 15:21:46 2016 +0900

    Merge pull request #213 from aqjune/assertsremain3
    
    Mem2Reg와 병합, opt failure / #150 이슈 해결

commit 6f31f670878b8bf5e4552b749e96a6998868b48b
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Tue Oct 25 14:46:08 2016 +0900

    Uncommentize GVN.cpp codes

commit f8c7172c171a671b5bdc4e972f2e1ffb342c09b6
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Tue Oct 25 14:17:02 2016 +0900

    Update dead_store_elim compilation error

commit c74af1231c4f71fc859e3a1dc248aa1b7c243396
Merge: fe05a7d 08722e2
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Tue Oct 25 14:15:26 2016 +0900

    Resolve confclits

commit 5e0f8411e22fadb6ca324f6dd4ee7180005a9cd1
Merge: 0063813 74b3216
Author: markshin <shin.dongyeon@gmail.com>
Date:   Tue Oct 25 13:49:28 2016 +0900

    Merge branch 'refactoring_debug' of https://github.com/SanghoonPark/llvm into refactoring_foldPHIbin

commit 0063813870dee69bbb1ecb8a691ddd4fad5a23b5
Merge: 9b04889 7f9a051
Author: markshin <shin.dongyeon@gmail.com>
Date:   Tue Oct 25 13:49:10 2016 +0900

    conflict fix cause by update new version of llvm

commit 74b32168279df584a5bd33ab9dce2576b79b37d3
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue Oct 25 13:46:53 2016 +0900

    Fix bug in properPHI

commit 7f9a051847f71ee0bb84372c9bfc33188175b082
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Oct 24 18:42:37 2016 +0900

    Fix bug of RenamePass hint generation in mem2reg

commit 9b04889d8e9ebfcb3e2f7e8fb5d1835d57baf47b
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Oct 24 18:19:09 2016 +0900

    Fix bug in RenamePass of mem2reg

commit 141d1033df3a1ab5e91238c9d7b6e9c4bdfbf35d
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Oct 24 00:46:36 2016 +0900

    Let llvmberry runtime options be enabled in clang as well.

commit c577fa3bf8163991fb3e90e4f0dc2e19f3c02f7f
Author: markshin <shin.dongyeon@gmail.com>
Date:   Sat Oct 22 22:11:47 2016 +0900

    fix undef phi condition

commit 671d374bb4420f6fabd883869de125ae8a813c0e
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Sat Oct 22 21:26:21 2016 +0900

    Fix bug in optimizing use inst in mem2reg

commit 08722e29886a08a0cc383e6203eae908f8d7feb0
Merge: fe2633f ba2187b
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Sat Oct 22 13:10:54 2016 +0900

    Merge pull request #212 from aqjune/assertsremain2
    
    #163에 있던 최적화 모두 옮김

commit fe2633feb52efad19f25bc6107cbe40126e07345
Merge: dd1afa8 f554a54
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Thu Oct 20 20:03:25 2016 +0900

    Merge pull request #214 from alxest/PRE
    
    Pre

commit f554a54a7838e2e88a79ef0163a6c8cc36fd0028
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Thu Oct 20 16:02:41 2016 +0900

    Update Hintgen.cpp

commit 083eb833c92b5ab0052807b1839ab46896e17dc2
Merge: dd1afa8 05b40c9
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Oct 20 15:37:43 2016 +0900

    Merge branch 'gvn_pre_hard_last' into PRE
    
    * gvn_pre_hard_last: (49 commits)
      Fix some GFail/VUnknown in Python
      Extend to cover pre-phi operand
      Change semantics of generateHintForPRE for next work
      Add getPHIResolved
      Revert "Add PrevPRETable and rename ArgForGVNPREIntro properly"
      Revert "Implement PrevPRETable"
      Implement PrevPRETable
      Use delete instead of eraseFromParent
      Add PrevPRETable and rename ArgForGVNPREIntro properly
      Add more debug prints
      Implement recursive case
      Revert "Use Var(CurInst) instead RHS(CurInst) for unity"
      Use Var(CurInst) instead RHS(CurInst) for unity
      Correct comment position
      Simplify nested if
      Rearragne code for VPHI case and VI case
      Add some more comments, remove code not needed
      Shorten lifetime of VI_evolving_next to prepare Phi case
      Separate PHI Node case
      Fix logical bug: change CurrentBlock to PhiBlock
      ...

commit 72b1b7130b865eb97e83a547199ec1f57b6ce7a3
Merge: d323791 1adebab
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Wed Oct 19 21:35:59 2016 +0900

    Merge branch 'refactoring_foldPHIbin' of https://github.com/markshin/llvm into refactoring_debug

commit d323791a869885ae623cd18893ddfcadda87f758
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Wed Oct 19 21:35:12 2016 +0900

    Optimize algorithm finding use of instructions

commit 1adebab9a4c8cc75c34d4b393ec63f88907851b8
Author: markshin <shin.dongyeon@gmail.com>
Date:   Wed Oct 19 18:56:01 2016 +0900

    fix bug in phiDelete and change worklist in propagatePerblock to edge

commit 71402bf36658986ef959182739676f2917e616fe
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue Oct 18 17:09:05 2016 +0900

    Search Dictionary rather than instruction's use

commit dd1afa802329777d9a97a54d2d9e22426ec185d0
Merge: 30dc720 dfbec98
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Tue Oct 18 04:08:17 2016 +0900

    Merge pull request #211 from kim-yoonseung/gvn_switch
    
    Handle switch in GVN

commit dfbec98d29cd510fd53b6b4fc89f1a04fd468965
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Mon Oct 17 12:36:47 2016 +0900

    Move effectful functions out of assert in GVN hintgen

commit cfd08bc055534f30285f2e0b895835fbb535cc15
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Oct 17 11:02:24 2016 +0900

    Save instruction's use information in Dictionary

commit fe05a7d79f2f6e1e93e61bd1273f8cc7b3a6867d
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri Oct 14 17:17:30 2016 +0900

    Add floating point NaN support

commit 88d52152fedab06ccda0e7ca583b1e01c02c29c4
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Oct 3 00:55:08 2016 +0900

    Remove bitcasstptr_const, gepzero_const

commit 892948e96c963c8a1bda84ad46993e583aa11936
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Oct 14 17:01:12 2016 +0900

    fix conflict

commit deed3a78c65ff12a52168bfeaa157bb7a43be68a
Merge: cfd99fc 5519442
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Oct 14 16:47:47 2016 +0900

    fix confict

commit cfd99fc26fcca0239dbbf757c8ad92330a48bbd0
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Oct 14 16:44:46 2016 +0900

    add typosition::make

commit 551944261e506e10eea1ac6eaa6aa01d4a48f450
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Oct 14 16:43:18 2016 +0900

    Add dictionary for store information of use instruction

commit 1f36e82b5857b0ac193f5cf143dcd0f36f559fc5
Merge: 6b807ff 786192c
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Oct 14 13:15:05 2016 +0900

    Merge branch 'refactoring_debug' of https://github.com/SanghoonPark/llvm into refactoring_foldPHIbin

commit 6b807ff8757a336a644c88b8615efb7c9112c74f
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Oct 14 13:14:32 2016 +0900

    debug message

commit 786192c2bd68d714c953e8b7673f54bfde485c5f
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Oct 14 13:13:17 2016 +0900

    Add Dictionary for saving return block

commit 534164eec5bb34c27bdab896de0fa6eb197b2ef7
Merge: 1bda2d6 42a539a
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Oct 13 19:30:38 2016 +0900

    Merge branch 'refactoring_foldPHIbin' of https://github.com/markshin/llvm into refactoring_debug

commit 42a539a7bf58f0d4daa265043d7a8a6c28224268
Author: markshin <shin.dongyeon@gmail.com>
Date:   Thu Oct 13 14:28:52 2016 +0900

    fix phidelete function, add another bit and ignore parameter

commit 1bda2d6730c8b165d06401c7f08715fb99ac9bd2
Merge: 7700c3e 5cf77ce
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Wed Oct 12 09:27:39 2016 +0900

    Merge branch 'refactoring_foldPHIbin' of https://github.com/markshin/llvm into refactoring_debug
    
    Conflicts:
    	lib/LLVMBerry/Dictionary.cpp

commit 7700c3e21526c689a9bedc02ab1f20110a0b3b0d
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Wed Oct 12 09:23:18 2016 +0900

    Save infrules for replacing

commit 5cf77cecabd7117a0dad7f63648b38e75d8aa29e
Author: markshin <shin.dongyeon@gmail.com>
Date:   Tue Oct 11 20:29:32 2016 +0900

    fix PHIdelete and PropagatePerBlock functions

commit 9b7f830f4d4bb19abde3bc7e6fcb9942cca9f0aa
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Oct 10 21:01:05 2016 +0900

    Add instructions to STructure.cpp , etc

commit 3418bfded3eb717ff6b236ece445fba5d47d8e8d
Merge: 73778a9 adc32ac
Author: Park, Sanghoon <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Oct 10 18:39:15 2016 +0900

    Merge pull request #1 from petrosyh/refactoring_debug
    
    resolve unknown

commit adc32acc212250d57f900bba30157e9caaa2f1b3
Merge: 71a94ab 73778a9
Author: petrosyh <yonghyun.kim@sf.snu.ac.kr>
Date:   Mon Oct 10 18:29:45 2016 +0900

    Merge branch 'refactoring_debug' of https://github.com/SanghoonPark/llvm into refactoring_debug

commit 71a94ab58d2ea4131cb7e0b153cb98fa503a1ffa
Author: petrosyh <yonghyun.kim@sf.snu.ac.kr>
Date:   Mon Oct 10 18:29:34 2016 +0900

    Solve problem that load is propagated to phinode

commit 73778a927484bee14b27959ac506882034946928
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Oct 10 18:17:23 2016 +0900

    Fix bug in SingleBlockAlloca of mem2reg, Add missing objects into replace hint

commit ba2187bead116f978b9705ca1eb9c6445b2058a0
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Oct 10 14:52:18 2016 +0900

    Undo dead_store_elim

commit e0622c686dcb2a075b046738f1da5dceedb06baf
Merge: 0acee9b 5ef175c
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Oct 10 14:23:53 2016 +0900

    Resolve conflict with Mem2Reg branch

commit 116c6af32baf82ac262176b0918bd15fee8c0a43
Merge: f7ec198 30dc720
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Oct 10 12:08:04 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring_debug
    
    Conflicts:
    	include/llvm/LLVMBerry/Hintgen.h
    	include/llvm/LLVMBerry/Structure.h
    	lib/LLVMBerry/Structure.cpp
    	lib/Transforms/InstCombine/InstCombineLoadStoreAlloca.cpp

commit f7ec198f3372a45df2f9ca394c08323e93f67e22
Author: markshin <shin.dongyeon@gmail.com>
Date:   Sat Oct 8 20:19:48 2016 +0900

    pull from sanghoon and delete unnecessary codes

commit 5453a8dddfe88fb39d458523192daad450a4db99
Merge: ace7572 1622d13
Author: markshin <shin.dongyeon@gmail.com>
Date:   Sat Oct 8 20:17:05 2016 +0900

    Merge branch 'refactoring_debug' of https://github.com/SanghoonPark/llvm into refactoring_foldPHIbin

commit ace7572ebfa6ad1eb4ae87c82287e839ec3e1361
Author: markshin <shin.dongyeon@gmail.com>
Date:   Sat Oct 8 20:16:34 2016 +0900

    fix the bugs in the undef phi propagation part

commit 1622d131fb1d0edd7039b0dd99aa75431083b92c
Merge: 4978290 843f6c2
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Oct 7 21:22:50 2016 +0900

    Merge branch 'refactoring_foldPHIbin' of https://github.com/markshin/llvm into refactoring_debug

commit 843f6c2565ef530ee321ab806db08010b8a78953
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Oct 7 20:56:33 2016 +0900

    undo undef phi propagation in rename

commit 4978290e0df6a34e8f6134ca2d0770bd337ce5f9
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Oct 7 18:14:18 2016 +0900

    Refactoring block iteration in mem2reg (initial success)

commit fdb8d7be171d8d829003236325379331367d6898
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Oct 7 16:18:07 2016 +0900

    fix bug for 5.implicit which gave wrong termIndex and fixing bug in the undef in the phi(not complete)

commit 0acee9b457e6d3fde69eb48d912aec42084ef399
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed Oct 5 03:32:24 2016 +0900

     Add two more optimizations - sext_trunc_ashr, ptrtoint_inttoptr

commit 8a23a370f68d60e3bc593f96f85dbc42ca1d46db
Author: markshin <shin.dongyeon@gmail.com>
Date:   Tue Oct 4 20:46:31 2016 +0900

    when there is undef incoming in the phi, change propagation condition between phi and load

commit 894f201b8ab170b76cec99516a2057427f2428e8
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Mon Oct 3 17:21:08 2016 +0900

    Handle switch in GVN

commit 30dc720f8e0655aa3721554573e6309c964d7e8d
Merge: 23e08d8 eeaf66b
Author: aqjune <aqjune@gmail.com>
Date:   Mon Oct 3 01:51:46 2016 +0900

    Merge pull request #208 from aqjune/assertsremain
    
    Load-load / Load-store 최적화 python/SPEC2006에서 검증 완료

commit eeaf66bc87bea92b7236a460fda49c44449f2df7
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Oct 3 00:55:08 2016 +0900

    Remove bitcasstptr_const, gepzero_const

commit 5ef175ca5082a8a3722d0bc4ea4fa8b15a3964ee
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Sep 29 15:28:42 2016 +0900

    Fix bug while propagate unique in Mem2Reg

commit d9abdae14d2d8732aa7b7b6ba0fed4c2a2b8850c
Merge: 701d1ce 013dd1a
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Sep 26 16:37:24 2016 +0900

    Merge branch 'refactoring_foldPHIbin' of https://github.com/markshin/llvm into refactoring_debug
    
    Conflicts:
    	lib/LLVMBerry/Hintgen.cpp

commit 013dd1ae0eeae9afded08077769380b678c3f427
Author: markshin <shin.dongyeon@gmail.com>
Date:   Mon Sep 26 16:35:38 2016 +0900

    fix bug socketmodule send_msg 84

commit 0bf3925da6d2f574400950cd6af65055efd54bdb
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Sep 26 14:29:45 2016 +0900

    Remove desc field in Infrule / propagate

commit 60368a6660d6150b84694caa83c90fb0f3faf0f7
Merge: cf0e820 8ac67d7
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Sep 26 14:04:40 2016 +0900

    Merge remote-tracking branch 'origin/assertsremain2' into assertsremain
    
    * origin/assertsremain2:
      Add admitted to call inst in sink_inst
      Works for SPEC2006
      Admit when src and tgt has different namedts

commit 701d1cea92344ef3e31b5ac6a3b3474f94190849
Merge: fa4575a 75c12a6
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Sep 26 13:47:56 2016 +0900

    Merge branch 'refactoring_foldPHIbin' of https://github.com/markshin/llvm into refactoring_debug
    
    Conflicts:
    	lib/LLVMBerry/Hintgen.cpp

commit fa4575aa5f84c753ee1312d101142068ebc06e75
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Sep 26 11:07:31 2016 +0900

    Add dictionary for block search in Mem2Reg

commit 8ac67d79743e5119adfb6934230ac566aabc1fd1
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Sep 26 02:31:03 2016 +0900

    Add admitted to call inst in sink_inst

commit aa8ef097fa69e3ca7eab8d7483fed6e7a816f232
Merge: f8cc9bf 23e08d8
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Sep 26 02:20:59 2016 +0900

    Merge branch 'refactoring' of https://github.com/snu-sf/llvm into assertsremain2
    
    * 'refactoring' of https://github.com/snu-sf/llvm:
      Admit when src and tgt has different namedts
      Uncomment PRE
      Remove obsolete hintgen code for GVN in hintgen.h and hintgen.cpp
      Remove bugs for GVN hintgen
      Refactor GVN for clonned cmp

commit f8cc9bf761118c8e437df5cfb8dba5d1f7b5903a
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Sep 26 02:20:51 2016 +0900

    Works for SPEC2006

commit 23e08d89232d72597c565438010f44efd6da8ce8
Merge: 25dd451 963949f
Author: aqjune <aqjune@gmail.com>
Date:   Sat Sep 24 18:16:54 2016 +0900

    Merge pull request #210 from petrosyh/dce_admitted
    
    DCE에 있는 Fail 및 Unknown 제거

commit 75c12a61c1ccc60f7e90707b481fc775e3bb4478
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Sep 23 19:05:14 2016 +0900

    fix bug for socket send_msg 30

commit c27fa142280ac44ee6f703137fa766aac9c66d9a
Merge: 6bf1c78 c1c29fa
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Sep 23 17:33:49 2016 +0900

    Merge branch 'refactoring_debug' of https://github.com/SanghoonPark/llvm into refactoring_foldPHIbin

commit c1c29fa8beaaa3332b282c25ac0255a520f9aba4
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Sep 23 17:31:21 2016 +0900

    Temporary commit

commit 963949f2eeb837aa5b6da4ed056b833771503013
Author: petrosyh <yonghyun.kim@sf.snu.ac.kr>
Date:   Tue Sep 20 16:16:23 2016 +0900

    Admit when src and tgt has different namedts
    
    reflect comment

commit cf0e8209744ee704f85d2987fe5f5cb8bb379ef9
Merge: b460f4f 25dd451
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Sep 19 17:37:17 2016 +0900

    Merge branch 'refactoring' of https://github.com/snu-sf/llvm into assertsremain

commit 25dd4510a76250fc5e66083af5ed7f16db1d4508
Merge: 36235f2 50b2e41
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Mon Sep 19 15:50:06 2016 +0900

    Merge pull request #207 from kim-yoonseung/gvn_refact
    
    GVN 힌트 생성 구현

commit b460f4f327ef6e8bc5a4c1824d3c11c80315dbc4
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Sep 19 13:41:56 2016 +0900

    Apply clang-format

commit 4446849c8f4998edd60907d7828ca974db4ddd78
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Sep 19 13:36:45 2016 +0900

    Correct load-load/load-store optimization , now  works perfectly in Python

commit 6bf1c783f55239f464d7a10b09f3c946684a05f2
Merge: de538de 91ed3ca
Author: markshin <shin.dongyeon@gmail.com>
Date:   Mon Sep 12 15:49:16 2016 +0900

    Merge branch 'refactoring_debug' of https://github.com/SanghoonPark/llvm into refactoring_foldPHIbin

commit 91ed3ca1c7029a0e398f46ce5ee4f0040060cfb8
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Sep 12 15:45:50 2016 +0900

    Fix bug in SingleBlockAlloca

commit de538deb591fd4e18c12b237fb23e0a7775e0423
Merge: 531f5af d10d7c0
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Sep 9 20:03:10 2016 +0900

    Merge branch 'refactoring_debug' of https://github.com/SanghoonPark/llvm into refactoring_foldPHIbin

commit d10d7c0d628a3e1a2e3524f118601858eab8cc5e
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Sep 9 20:01:44 2016 +0900

    Fix bug in replace transitivity tgt

commit 4cc8bc6d9002ba78a48cb0afef38bc8c8ca7d6a2
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Sep 9 13:58:16 2016 +0900

    Add Dictionary to save diffblock information

commit 36235f2247cd56b27bb297b36a5ddb126ed743d1
Merge: fcee9a8 d4520d7
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Thu Sep 8 13:50:08 2016 +0900

    Merge pull request #205 from aqjune/assertsremain
    
    zext_xor, select_bop_fold, and_or_const2 VFail 수정 및 최적화 12개 옮김

commit d4520d77887fd43c8e675e655b537be11d9d28bf
Merge: 6e8ac32 fcee9a8
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed Sep 7 15:53:43 2016 +0900

    Merge branch 'refactoring' of https://github.com/snu-sf/llvm into assertsremain

commit 531f5afb9605b2546b79b3a7b43a2d89ea01271f
Merge: efcc785 3c7a745
Author: markshin <shin.dongyeon@gmail.com>
Date:   Tue Sep 6 14:29:10 2016 +0900

    Merge branch 'refactoring_debug' of https://github.com/SanghoonPark/llvm into refactoring_foldPHIbin

commit 3c7a745c43a610be201cdebcad9a055a12b82845
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue Sep 6 14:24:38 2016 +0900

    Modify RenamePass hint generation code

commit efcc785ebb85aae73b14e1818e1b5a436a70b195
Merge: 4f76691 0bffa5a
Author: markshin <shin.dongyeon@gmail.com>
Date:   Tue Sep 6 13:35:57 2016 +0900

    Merge branch 'refactoring_debug' of https://github.com/SanghoonPark/llvm into refactoring_foldPHIbin

commit 428815793434b89aafd7c866e4803ce20a189c45
Merge: efbfd4d 4f76691
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Sep 5 20:17:13 2016 +0900

    Merge branch 'refactoring_foldPHIbin' of https://github.com/markshin/llvm into refactoring_debug

commit efbfd4d70e1f9e3ff6748d46660a23147cf46bb1
Merge: 4a0c8bd fcee9a8
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Sep 5 20:14:37 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring_debug
    
    Conflicts:
    	lib/Transforms/Scalar/GVN.cpp

commit 4f766915873ccb8f6ab76b512e6d5366ebe4b663
Author: markshin <shin.dongyeon@gmail.com>
Date:   Mon Sep 5 20:13:29 2016 +0900

    working one problem that load instruction erased before store instrution on the other side

commit 4a0c8bd24f5bcbcbfe923217ef815581d36cc5d5
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Sep 5 19:54:14 2016 +0900

    Fix bug in traversal algorithm in Mem2Reg hint generation

commit 50b2e41f914b372660e840bed06794ecd0a77117
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Fri Sep 2 17:03:44 2016 +0900

    Uncomment PRE

commit 816a11f8718efea9004dd0b0133ab363bebb51ad
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Fri Sep 2 16:52:32 2016 +0900

    Remove obsolete hintgen code for GVN in hintgen.h and hintgen.cpp

commit 241b9aeefc5bfda8e506584801f386cae85383fe
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Fri Sep 2 16:30:56 2016 +0900

    Remove bugs for GVN hintgen

commit 32c70e971685ad7f291551ad250a8a59aa8d7081
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Fri Aug 26 18:55:24 2016 +0900

    Refactor GVN for clonned cmp

commit 05b40c9217fd4fc6a01efcf9adb208f323b52bee
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Fri Sep 2 04:39:08 2016 +0900

    Fix some GFail/VUnknown in Python

commit fcee9a88a0543d9c8a7bba9c75180303677f785e
Merge: 5b3fcdc 776211c
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Thu Sep 1 13:22:59 2016 +0900

    Merge pull request #206 from kim-yoonseung/bop_comm
    
    Bop Commutativity 통합

commit 0bffa5ac8b10db69b403d8467d7c08cd1f7f1354
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue Aug 30 13:02:55 2016 +0900

    Remove tag from unique

commit 860b46f3d536935e9fc716748923fc2f781617ba
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sun Aug 28 19:38:16 2016 +0900

    Extend to cover pre-phi operand

commit 3b9912c03cfffbd2a8bf2e898f24b64bdf0a85a8
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sun Aug 28 01:15:15 2016 +0900

    Change semantics of generateHintForPRE for next work
    
    To prepare for the case where CurInst has an operand that is PHINode.
    I want to use getPHIResolved function just made, but using it with RHS
    will not have any meaning because RHS will only pass the name of
    register, while it does not actually appear in IR.

commit c09cc27ee525f2e01bed1d01da3c808875e17392
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sun Aug 28 01:05:40 2016 +0900

    Add getPHIResolved

commit e1a21f82520607f533795f43ba0bc100150e77b0
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sat Aug 27 17:40:38 2016 +0900

    Revert "Add PrevPRETable and rename ArgForGVNPREIntro properly"
    
    This reverts commit 3220db4d52d3820dae5e8f5ea38b59cbb508b13c.

commit 0f6ca5dd9eab7ed0d89b6496ad8c3eb8d3a359a9
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sat Aug 27 17:40:30 2016 +0900

    Revert "Implement PrevPRETable"
    
    This reverts commit ba2ff785be5104bfdbd0ff4487f979561ad3f3c6.

commit ba2ff785be5104bfdbd0ff4487f979561ad3f3c6
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sat Aug 27 16:08:23 2016 +0900

    Implement PrevPRETable

commit bbe1b04705fe0ead03ec141a78ae34dc25592000
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Fri Aug 26 22:25:37 2016 +0900

    Use delete instead of eraseFromParent

commit 3220db4d52d3820dae5e8f5ea38b59cbb508b13c
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Fri Aug 26 20:33:26 2016 +0900

    Add PrevPRETable and rename ArgForGVNPREIntro properly

commit 7df35a478173f9965281962daad02fb617055ea3
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Fri Aug 26 20:14:22 2016 +0900

    Add more debug prints

commit 4d70bd073a5d5e67961dbcbea0c670b850c629f5
Merge: d05c930 5e7cbdf
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Aug 26 17:31:01 2016 +0900

    fixed conflict from sanghoon's

commit d05c9305337bf1e626e0a707cf78fc39cf39eb74
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Aug 26 17:25:43 2016 +0900

    fixed the way to find termIndex

commit 5e7cbdf02549af6cc04988322f1b335bfe79140b
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Aug 26 17:16:59 2016 +0900

    Fix dictionary of terminator indices

commit 6481790bd879f975d84aaf40723f399e742fad9a
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Aug 26 16:57:02 2016 +0900

    Merge remote-tracking branch of markshin

commit 74e461bb74457ccd5da5120866182f77cf145629
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Aug 26 14:53:42 2016 +0900

    Use Unique instead of Alloca

commit bff1d13e96ebc351c86a0081c59156b7d5bc7a6e
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Aug 26 12:13:51 2016 +0900

    checking for visitedblocks have been fixed

commit 4e5820d4315201057cb14249a3edd4f0e05f3636
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Fri Aug 26 01:28:13 2016 +0900

    Implement recursive case
    
    Fix semantics of generateHintForPre, and change accordingly.
    Remove PrevPRE size checking.
    Add propagate in VPHI case to complete the logic.

commit be1214c053062c2e519666360bf0e6d655413bb3
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 25 23:21:51 2016 +0900

    Revert "Use Var(CurInst) instead RHS(CurInst) for unity"
    
    This reverts commit 7244c4954a31b161531572d5a5eee7b11bc6cd95.

commit 7244c4954a31b161531572d5a5eee7b11bc6cd95
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 25 22:28:27 2016 +0900

    Use Var(CurInst) instead RHS(CurInst) for unity

commit 02c0e5c4599a517d8af8b15e1a100d237712d434
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 25 22:21:25 2016 +0900

    Correct comment position

commit 3e1c4867a34496efd22aaabda10cf8e24c2a73a4
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 25 22:20:09 2016 +0900

    Simplify nested if

commit 9332895782494ea634631a7a78757eb97c6459d3
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 25 22:15:44 2016 +0900

    Rearragne code for VPHI case and VI case

commit f8b3732d9299df12e1529dff87f91b0b5ca1dcd2
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 25 22:10:22 2016 +0900

    Add some more comments, remove code not needed

commit 87b71eb0b1158ab5cafff99a83cd965bb02a7a3e
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 25 21:43:26 2016 +0900

    Shorten lifetime of VI_evolving_next to prepare Phi case
    
    Also add some comments for readability.

commit 693727d101afa62232fab88eef8438ebe769cb0f
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 25 21:06:48 2016 +0900

    Separate PHI Node case

commit 35fc641485e9bbdcb37f494e7944bfc52b54f94b
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 25 20:57:58 2016 +0900

    Fix logical bug: change CurrentBlock to PhiBlock

commit 590d90b428a634ea3faaf5a1d2ef97c457eb6e8c
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 25 10:34:00 2016 +0900

    Separate generateHintForPRE

commit 7480d90ce723683b402ffd9000358c6848a4a48a
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 25 10:13:07 2016 +0900

    Pull variable out of capture to prepare for recursion

commit 0000fb802cd682ac9765b71e3d7c9072d8ff9c3f
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 25 01:19:15 2016 +0900

    Remove buildPredMap

commit 11b4aeb6144dce1d3566f53fcb3f35d16da2fb95
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 25 01:18:38 2016 +0900

    Remove predMap, PREInstr to prepare for recursion

commit 78485750007b2d4fb3ed22966e7ceaca9fcf893c
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed Aug 24 23:02:16 2016 +0900

    Unite hint generation for RAUW

commit 9c8dae0619d672df8c5c02b806e74f8fe6000719
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed Aug 24 22:41:22 2016 +0900

    Substitute isa with dyn_cast

commit 3c36346fa04d0de57f1757e446ae88153ae2ef7f
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Tue Aug 23 20:45:29 2016 +0900

    Solve admitted GEP inbounds remove case

commit e20bd3e5d845b510bb7732d6502f13a92b87cb93
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Tue Aug 23 16:21:05 2016 +0900

    Separate gep inbounds cases

commit 776211cc8782e4e57caa5b8cbfad796bc2c69c9e
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Mon Aug 22 17:20:29 2016 +0900

    Remove commutativity of each bop

commit 5a5afc30b04baadd6220b9983de2581f8dd160fa
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Mon Aug 22 16:24:47 2016 +0900

    Unify bop's commutativities into BopCommutative and change ApplyCommutativity according to this

commit 28478dae3c99b2fdb7505c5ad402c9ca75e4d0c5
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Mon Aug 22 15:20:28 2016 +0900

    Revert "Add hasSameOperands to separate gep inbounds removal"
    
    This reverts commit b536c7051ab366afb014d8af700754404dabdef5.

commit b536c7051ab366afb014d8af700754404dabdef5
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Mon Aug 22 14:17:52 2016 +0900

    Add hasSameOperands to separate gep inbounds removal
    
    However, gep inbounds is not included in operands list, and this
    approach didn't work.

commit 685e2e2524a4b9b53cbbfe2afc47b9b57ca33239
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sun Aug 21 23:36:43 2016 +0900

    Add isFromNonLocalLoad

commit f7e70f6283a8bee2b91bfb1bf05b92151ce37d37
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sun Aug 21 01:58:46 2016 +0900

    Use same hint generation as in GVN_PRE

commit 6e8ac3288d030bb4c3b9b760e186a98343f7d0b3
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sun Aug 21 01:18:11 2016 +0900

    Add optimizations :
     select_icmp_eq
     select_icmp_ne
     select_icmp_eq_xor1
     select_icmp_eq_xor2
     select_icmp_ne_xor1
     select_icmp_ne_xor2
     select_icmp_sgt_xor1
     select_icmp_sgt_xor2
     select_icmp_slt_xor1
     select_icmp_slt_xor2
     select_icmp_gt_const
     select_icmp_lt_const

commit 746961517fb3df3231a2b4b8141515633224468e
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Aug 20 02:44:12 2016 +0900

    Modify validation fails in zext_xor, select_bop_fold, and_or_const2

commit 0247bbaaa305bb76f2fb938248a272be262dc795
Merge: 4286018 5b3fcdc
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri Aug 19 15:07:12 2016 +0900

    Merge branch 'refactoring' of https://github.com/snu-sf/llvm into assertsremain

commit 42860189d375856318ee69fd9d178557d6ce5b9f
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri Aug 19 15:07:02 2016 +0900

    Modify ZextXor

commit fb3da990caef2a08502c77537aa33f1cd51969d8
Merge: bd2f458 5b3fcdc
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Fri Aug 19 01:44:30 2016 +0900

    Merge branch 'refactoring' into gvn_pre_hard_last
    
    * refactoring:
      Unify passdictionaries of GVN and PRE
      Revert our GVN processBlock iterator manipulation
      Apply GEP bugfix in llvm trunk r275532
      Add Infrule IcmpInverseRhs
      Add infrules AndTrueBool & OrFalse
      Change ReplaceRhs to substitute & Add Propaga InInst to Block that dominated by InInst
    
    Conflicts:
    	lib/Transforms/Scalar/GVN.cpp

commit bd2f4588fb7e87ae674b08837cf005de9d47d9f5
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Fri Aug 19 01:36:36 2016 +0900

    Add some debug prints, replace assert by VFail

commit 5b3fcdc548d62b5a1b4d7eccd9de31a4ea044cf3
Merge: 76c1b64 a416a94
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Fri Aug 19 01:30:20 2016 +0900

    Merge pull request #204 from kim-yoonseung/gvn_refact
    
    GVN 관련 Infrule 추가 및 여러 수정

commit a416a94d5c2dbbbd3e9a48d2d63e8b52638cfcd3
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Fri Aug 12 13:33:50 2016 +0900

    Unify passdictionaries of GVN and PRE

commit b4a338954134c74a8fc55882e78cd67725fb352a
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Fri Aug 12 13:26:30 2016 +0900

    Revert our GVN processBlock iterator manipulation

commit 645fc0799fb3641d715ecffaf35f93002d575f2e
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Fri Aug 12 13:19:30 2016 +0900

    Apply GEP bugfix in llvm trunk r275532

commit 7d5f1f8f025be9d5e7b864654bbc55afa5ddf8c9
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue Aug 16 15:41:45 2016 +0900

    Add Infrule IcmpInverseRhs

commit 0b3c96b79675bae97335cff4e64ef3e3d422bfde
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue Aug 16 15:39:57 2016 +0900

    Add infrules AndTrueBool & OrFalse

commit 76c1b6456bc10aaf6dbbdbe863a2b660fccc751d
Merge: f5c7178 4c7c761
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue Aug 16 15:10:42 2016 +0900

    Merge pull request #202 from petrosyh/new_foldphi
    
    FoldPhiBin에서 Assertion Fail 제거

commit 0b801cad1744775c2bf3ce12da37c949aa652a3c
Merge: 6d7d7b3 433a77f
Author: markshin <shin.dongyeon@gmail.com>
Date:   Tue Aug 16 11:14:12 2016 +0900

    Merge branch 'refactoring_debug' of https://github.com/SanghoonPark/llvm into refactoring_foldPHIbin
    
    Conflicts:
    	lib/LLVMBerry/Hintgen.cpp

commit 433a77f1e97c1db94b28294d7e699a7bf6f7547c
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue Aug 16 10:54:13 2016 +0900

    Make traversal algorithm in mem2reg hint generation more efficiently

commit 6a5cee053fc58007a473e627ecbefce2cfe05bda
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sun Aug 7 18:21:27 2016 +0900

    Pop out hint generation for replaceAllUsesWith

commit 326b651b7cf57f90e5941f66a98aae047b873300
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Fri Aug 5 23:17:02 2016 +0900

    Fix all GFail

commit a9911a4c00341feca9758fd0cdf1babea8edf92e
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Fri Aug 5 22:03:09 2016 +0900

    Change return to assert false

commit b62a6f61956d234c88d764af2d66c712f1987845
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Fri Aug 5 21:37:18 2016 +0900

    Move PrevPRE calculating logic

commit 64d572a84a1f949aa02c8a02b6b43fa79f21e36b
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Fri Aug 5 12:28:56 2016 +0900

    Separate PREAR

commit 1faf88185112230dd37d1e39ddcc21d93559d0c7
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 4 23:01:49 2016 +0900

    Fix one GFail, only add hint when needed.
    
    It may be hard to read diff, because of clang-format.
    The only actual difference is adding "if (PrevPRE.size()) {".

commit 66a4408eb05149fbd563e939e8712ca75c8fb1b8
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 4 22:59:29 2016 +0900

    Add some debug print

commit 7be55183efc65494ba6e95262baa6f9ff0add872
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 4 21:21:27 2016 +0900

    Fix most VUnknown and VAssertionFailure
    
    Remaining VUknown seems to related with lowerswitch, it fails on finding
    basicblock specified in the hint file, in actual fdef.

commit 159bc4532e9d431fb3605c89a7ebdfa5de061bdd
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 4 21:07:44 2016 +0900

    Fix bug on VU end position, move comment correctly

commit 71be0c3958a9f04b6d348ccacb0bfb6539ce5931
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 4 19:18:30 2016 +0900

    Fix assertion failure on setReturnCode

commit 2ba32ef3a454c92ab21d4f84a9a0643e086b2d70
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 4 19:08:14 2016 +0900

    Identify unknown cases
    
    Now, Unknown decreased from 295 to 150 in speccpu

commit e06f68f56474d6ceebfca1f49c0001a1ef83cd0b
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu Aug 4 18:20:48 2016 +0900

    Fix most GFail
    
    Now 2 GFail occurs in speccpu, only within GVN_PRE.
    These are from "assert("Um.. what is this case??" && false);".

commit e6cb4c023f427897f9b9c7576494fb126fe2d01b
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed Aug 3 22:34:41 2016 +0900

    Remove some asserts

commit 5a870e89260290e2406e1cf84fb34f7fb501fd5d
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed Aug 3 22:07:55 2016 +0900

    Fix assertion fail, it may be better to unite pass

commit d84366e07d285e657cd45a1c7304e78357596cdb
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed Aug 3 21:38:50 2016 +0900

    Sum up VU end
    
    Now the reader may be easier to check actually validation occurs here.
    VU::Begin cannot be summed up because of whitelisting

commit 4ca2f2b7eb81a495242b8f088755f37f822a43d9
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed Aug 3 14:22:10 2016 +0900

    Refactor the code
    
    Put off classification logic (GVN_PRE or GVN_PRE_hard) out of single VU,
    and separate GVN_PRE and GVN_PRE_hard to be separate VU.

commit 4c7c761524a5dc67febcbf0638e500e19aad11b7
Author: petrosyh <yonghyun.kim@sf.snu.ac.kr>
Date:   Sat Aug 13 09:21:01 2016 +0900

    Change ReplaceRhs to substitute & Add Propaga InInst to Block that dominated by InInst

commit 6d7d7b37f7e4a1a2e072c6fa95abbe13adf2a452
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Aug 12 17:17:42 2016 +0900

    fix bug in phi delete function

commit 49042d72e285719c22a4be332f7fd17690e7e8f7
Merge: e08122b e974ae9
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Aug 12 11:21:06 2016 +0900

    Merge branch 'refactoring_debug' of https://github.com/SanghoonPark/llvm into refactoring_foldPHIbin
    
    Conflicts:
    	lib/LLVMBerry/Dictionary.cpp

commit e08122b5d37dfdd4ccd3277afa1df4c912b2a65e
Merge: 032a52a f5c7178
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Aug 12 10:47:55 2016 +0900

    Merge branch 'refactoring' of https://github.com/snu-sf/llvm into refactoring_foldPHIbin
    
    Conflicts:
    	include/llvm/LLVMBerry/Dictionary.h
    	lib/LLVMBerry/Dictionary.cpp
    	lib/LLVMBerry/Infrules.cpp
    	lib/Transforms/Scalar/GVN.cpp

commit 032a52aacd812d3a5fd03cbb85e2324d19a4568e
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Aug 12 10:39:09 2016 +0900

    pull upstream

commit 73e9e8138fd06533f729d69902348d77d8181be1
Author: markshin <shin.dongyeon@gmail.com>
Date:   Thu Aug 11 14:10:41 2016 +0900

    erase unneccessary infrule

commit fe5db7e7c0f9890b7d336c6585f8fe8a19a0f6e8
Merge: 4109e59 f5c7178
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Aug 11 13:30:05 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring_debug
    
    Conflicts:
    	lib/LLVMBerry/Hintgen.cpp

commit 4109e5901e694f185e9eaec1108d9b77b9d9c396
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Aug 11 13:29:11 2016 +0900

    Modify properPHI to prevent irrelevant propagate

commit f5c717820b93e37ae76ab38b32a624bc80af6f7f
Merge: dcbee4b b832ac2
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Wed Aug 10 11:54:46 2016 +0900

    Merge pull request #198 from aqjune/assertsremain
    
    add_associativity 최적화 확장, const_int 에러 수정, intrude()가 pass whitelist 참조하도록, 최적화 10개 추가

commit e974ae9123fbd64385653960476cc5fa707bec82
Merge: be0a689 dcbee4b
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue Aug 9 14:14:24 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring_debug
    
    Conflicts:
    	lib/LLVMBerry/Dictionary.cpp

commit b832ac2f78c56dd74203664091c343019d2172b8
Merge: 4460ec1 dcbee4b
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Tue Aug 9 13:55:23 2016 +0900

    Resolve conflicts

commit dcbee4b67e7fa3b207fa1025eb5c75a36b765c0b
Merge: 3eb8b06 ea34a06
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue Aug 9 12:10:52 2016 +0900

    Merge pull request #201 from Ailrun/SwitchPRNew
    
    Switch Instruction 추가

commit ea34a069c74ac5ac8f88599314e27e424b3b2dfd
Author: ailrun <jjc9310@gmail.com>
Date:   Mon Aug 8 12:14:19 2016 +0900

    Add insn_switch binding to llvm
    
    Add ocaml bindings of SwitchInst Class at Instructions.h
    to following files
    
    - Core.h
    Declaration of wrapper functions
    - Core.cpp
    Definition of wrapper functions
    - llvm_ocaml.c
    Definition of real binding functions using wrapper functions
    - llvm.ml
    FFI with binding functions
    - llvm.mli
    Interface for binding functions

commit be0a689a05edd3ed7859eac9424402cf06f53bcc
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Aug 8 17:05:33 2016 +0900

    Apply makeAlignOne to Load, Store, Alloca Instructions in Mem2Reg

commit db34e50dc1530992359e7010e122cced47dea3bb
Merge: 7800bbe 7e2dfa2
Author: markshin <shin.dongyeon@gmail.com>
Date:   Mon Aug 8 16:41:49 2016 +0900

    Merge branch 'refactoring_foldPHIbin' of https://github.com/markshin/llvm into refactoring_foldPHIbin

commit 7800bbe9ebc19351d2c74ee29cf898e5f294224a
Merge: 6428d75 1bf5acf
Author: markshin <shin.dongyeon@gmail.com>
Date:   Mon Aug 8 16:35:57 2016 +0900

    fix conflict

commit 3eb8b0682aa67cc954c94c6b94552325c13c2933
Author: naong606 <naong606@naver.com>
Date:   Mon Aug 8 16:18:48 2016 +0900

    Replace setReturnCode with setIsAborted (#200)

commit ac56f9b29cf451dd2cb46a22bd7d6c4b19a47746
Merge: 2c39af7 4757c47
Author: aqjune <aqjune@gmail.com>
Date:   Mon Aug 8 16:00:00 2016 +0900

    Merge pull request #196 from naong606/refactoring
    
    Validate AndXorConst

commit 1bf5acfd82d4c53ed9bd88a2ce9c3cc03975fd0a
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Aug 8 13:16:50 2016 +0900

    Fix bug in Mem2Reg

commit 4757c47e9ed5e4929937678bf0fd6e3ab22ff8b6
Author: naong606 <naong606@naver.com>
Date:   Fri Aug 5 16:26:21 2016 +0900

    Validate AndXorConst
    
    Change addCommands with propagateMaydiffGlobal

commit 4460ec18be53d8bb2f426294dc0b6d6b6a26d982
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Aug 8 03:57:33 2016 +0900

    Apply commutativity in icmp_eq_srem, icmp_ne_srem
    
    Clang format

commit 6df587a982368accfd15b7852a87c22e54566a77
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sun Aug 7 22:14:30 2016 +0900

    Modify const_int value from int to string

commit 6903a629f170fc690adba27f838ef941d25a538f
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sun Aug 7 18:09:01 2016 +0900

    Let global intrude consider pass white list

commit c7751d62eb7e995b03686d79af7fd25b6874f1f6
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sun Aug 7 03:07:53 2016 +0900

    Add hint generation code for following optimizations :
    icmp_eq_sub
    icmp_ne_sub
    icmp_eq_srem
    icmp_ne_srem
    icmp_eq_add_add
    icmp_eq_sub_sub
    icmp_eq_xor_xor
    icmp_ne_add_add
    icmp_ne_sub_sub
    icmp_ne_xor_xor
    
    Expand associativity_add to bop_associativity

commit 7e2dfa2b81c7cc7bc9b179c415aef9ecebb57f49
Author: markshin <shin.dongyeon@gmail.com>
Date:   Sat Aug 6 21:20:39 2016 +0900

    lessundefTgt and PHINode delete pruning

commit 2c39af771d66b63502a881605fed9bce12d56f91
Merge: 82fe6f1 e25d4f2
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Fri Aug 5 18:00:20 2016 +0900

    Merge pull request #185 from aqjune/assertsremain
    
    InstCombine에서 assertion 에러 해결, shift instsimplify 구현

commit e25d4f245eea2e4c4ae620d8cd799f2d6dcecede
Merge: f18f951 82fe6f1
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Thu Aug 4 19:59:01 2016 +0900

    Merge branch 'refactoring' of https://github.com/snu-sf/llvm into assertsremain

commit 82fe6f14c1d70b902a3aedf1e21de6b987c94f52
Merge: 19e4ac5 97d091a
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Thu Aug 4 15:31:39 2016 +0900

    Merge pull request #173 from petrosyh/sinking-inst
    
    Sink Instruction 에러 수정

commit 6428d751b4333e07ba765e6dd24252b2c2cb279f
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Aug 4 13:49:30 2016 +0900

    Fix bug in mem2reg PHI

commit 19e4ac586398a35541e8208d4381c49c0f3aaf95
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Wed Aug 3 20:59:27 2016 +0900

    Remove abort to fix failure on whitelist (#193)
    
    제가 추가했던 코드이고, 간단한 수정이니, 제가 머지할게요

commit 97d091ac8d5cf8a69d9026a23784be2b20474037
Merge: 1acdbd6 1ca6b7f
Author: petrosyh <yonghyun.kim@sf.snu.ac.kr>
Date:   Wed Aug 3 15:17:26 2016 +0900

    Resolve conflict with refactoring

commit 1ca6b7f2967a43571b470828d9a920d032dfa113
Merge: d3ee40f fcd1418
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Wed Aug 3 13:23:34 2016 +0900

    Merge pull request #176 from kim-yoonseung/refactoring_passdict_globintr
    
    Add PassDictionary and global intrude

commit fcd1418e8fd082bd4f9159517ecb1294e562a42e
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Wed Aug 3 12:55:46 2016 +0900

    Use setIsAborted instead of setReturnCode in GVN

commit 2fcd26dab4f6b8740fff76adca563ed346989da6
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Thu Jul 21 12:15:58 2016 +0900

    Fix bugs: set PassDictionary::_Instance to nullptr after PassDictionary::Destroy. Also modify GVN hintgen code.

commit 76414d701c1b91bd17c7fa5e8706eab9a3f7607f
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Wed Jul 20 19:33:11 2016 +0900

    Add PassDictionary and global intrude

commit f0e52e525751c8cb51fe4bd361ac4cc5b8adec89
Merge: 0a0b725 86a8c23
Author: markshin <shin.dongyeon@gmail.com>
Date:   Wed Aug 3 12:03:59 2016 +0900

    Merge branch 'refactoring_debug' of https://github.com/SanghoonPark/llvm into refactoring_foldPHIbin
    
    Conflicts:
    	lib/LLVMBerry/Dictionary.cpp
    	lib/LLVMBerry/Hintgen.cpp
    	lib/Transforms/Utils/PromoteMemoryToRegister.cpp

commit 0a0b725bc4302d583f05f42c0018efef0ba46b4c
Author: markshin <shin.dongyeon@gmail.com>
Date:   Wed Aug 3 11:58:31 2016 +0900

    rename pass corner case

commit 86a8c23cd9462034cc7d6cdd6f5cf7f5c4655dd7
Merge: 67333ea d3ee40f
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Wed Aug 3 11:25:18 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring_debug
    
    Conflicts:
    	lib/LLVMBerry/ValidationUnit.cpp

commit 67333ea964f83cad6d4b9a75d6f035b259fc19b7
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Wed Aug 3 11:20:28 2016 +0900

    Temp

commit d3ee40f01ec3661668ee7a4f7d6aa56d3667f753
Merge: e30bb4c 4fdffb1
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Tue Aug 2 15:47:50 2016 +0900

    Merge pull request #178 from alxest/vu_assert_new
    
    Set Validation Result

commit 4fdffb1057c4f597d2c9ea5106b6b704e76b3e1d
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Tue Aug 2 15:30:12 2016 +0900

    Use enum instead of int

commit b864a326c85ac5b481d2917c6c5c74c54958ed6a
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sun Jul 31 19:33:34 2016 +0900

    Rename ValidationResult into ReturnCode
    
    Related issue: https://github.com/snu-sf/llvm/pull/178#discussion_r72039062

commit eda6afa039236fd5db66f4056daaaea7cc783789
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sat Jul 23 19:49:20 2016 +0900

    Remove outdated function
    
    This functionality should now replaced by VALIDATION_RESULT, it does not
    rely on unsafe string comparison

commit e8d8758697f539d64ec6269910242ab32196ce7f
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sat Jul 23 19:43:42 2016 +0900

    Add VALIDATION_RESULT in CoreHint
    
    Related issue/PR: https://github.com/snu-sf/llvm/pull/149,
    https://github.com/snu-sf/simplberry/issues/176

commit e30bb4cd53b8ca5926be7de5a13a150c66a69d45
Merge: cd02c88 762b5df
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Tue Aug 2 15:04:14 2016 +0900

    Merge pull request #191 from alxest/rename_return_code
    
    Rename return code

commit 762b5df25c1b6039ef262059f57240a8386ef623
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Tue Aug 2 14:56:17 2016 +0900

    Add assert

commit 169b94c3c991f60a83ae3caf7bd3603b41b710bc
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sun Jul 31 17:26:57 2016 +0900

    Refactor to enjoy composability

commit c6697a0d11c370e7353308cd615849ae37f8d6c0
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Tue Aug 2 14:47:48 2016 +0900

    Replace enum RETURN_CODE with bool isAborted
    
    Related issue: https://github.com/snu-sf/llvm/pull/178#discussion-diff-72039062

commit 4a0a7f2cb35b1835f317e4eda8545b433317360b
Merge: 9584eeb cd02c88
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue Aug 2 00:39:32 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring
    
    Conflicts:
    	include/llvm/LLVMBerry/Infrules.h
    	include/llvm/LLVMBerry/Structure.h
    	lib/LLVMBerry/Hintgen.cpp
    	lib/LLVMBerry/Infrules.cpp
    	lib/LLVMBerry/Structure.cpp

commit cd02c88b728563d31033b81f8ce49180b9c7f381
Merge: 4b117f0 b023076
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Mon Aug 1 18:33:23 2016 +0900

    Merge pull request #190 from Ailrun/InstCombinePR
    
    Add InstCombine and_or_not1

commit 9584eeb61847be75dbceb1fc1fab01eb0ac5e7d4
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Aug 1 16:23:07 2016 +0900

    temp

commit 4b117f06f4da39b3d2811b77107be0a9217d479c
Merge: d1185a9 c3b01c3
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Mon Aug 1 15:10:16 2016 +0900

    Merge pull request #189 from alxest/cleanse_some_code
    
    Cleanse some code

commit b02307633bf6ebf89b4f3e3a270914c2bcd595b3
Author: ailrun <jjc9310@gmail.com>
Date:   Mon Aug 1 14:06:02 2016 +0900

    Fix applyCommutativity function
    
    Fix bugs at switch statement of target branch of applyCommutativity.

commit edfb2403bef028df69fb8dd43a8da06037468366
Author: ailrun <jjc9310@gmail.com>
Date:   Mon Aug 1 11:20:55 2016 +0900

    Update InstCombine optimizations and_or_not
    
    Add the other case of and_or_not1
    (A | (~B)) & B = A & B
    
    Remove and_or_not2 comment

commit c3b01c3f59f33357eb7029289f922de167cb1f23
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sun Jul 31 21:37:44 2016 +0900

    Cleanse some code

commit c18d2c4728997245bdfafb83b9ca71629f4bc366
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Jul 29 18:35:59 2016 +0900

    Temporary

commit f18f951cb252c0181aa746b4e59c8babb4587721
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Jul 28 07:13:38 2016 +0900

    Add function to update private member of some infrules and structures
    
    Modify format in Dictionary.cpp

commit 82bcb689401052b0d5565d4ad0fa21518855c15b
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Wed Jul 27 18:55:04 2016 +0900

    Add Infrule LessthanUndefTgt

commit d1185a9fd721c0618b6edda031bc10e748a2e001
Merge: 57a26cc e0eaf02
Author: aqjune <aqjune@gmail.com>
Date:   Thu Jul 28 17:12:10 2016 +0900

    Merge pull request #183 from SanghoonPark/ModifyInfrule
    
    Add function to update private member of some infrules and structures

commit 57a26cc5c029afd9d7aefbbde31a7e3b838f4206
Merge: 8401d4f bd538cb
Author: aqjune <aqjune@gmail.com>
Date:   Thu Jul 28 15:36:45 2016 +0900

    Merge pull request #182 from SanghoonPark/AddInfrule
    
    Add infrule LessthanUndefTgt

commit 79caff8bbcc96f5518bd8f8f8321d72385eb4def
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Thu Jul 28 15:29:59 2016 +0900

    Let and_de_morgan hint gen abort when the type is vector ty

commit e0eaf02e81464695bde4efb85fd6ee827d267bca
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Jul 28 07:13:38 2016 +0900

    Add function to update private member of some infrules and structures

commit bd538cb51a5cbcbdf7f56db84f4212f4aa32907b
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Wed Jul 27 18:55:04 2016 +0900

    Add Infrule LessthanUndefTgt

commit 8f0070f7c32a6e177c1b64045f304568557632aa
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed Jul 27 18:48:59 2016 +0900

    Let inst simplify hint gen abort when types are int vector

commit b0fd318cbfc0fbf8fcc7a32b056960db6ca7d5f6
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed Jul 27 16:27:21 2016 +0900

    Add shift simplify hint generation code
    
    Apply clang

commit bd1044f724dd111578dd5cbb3684850f116f44ac
Merge: e4e42be 8401d4f
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed Jul 27 14:31:25 2016 +0900

    Merge branch 'refactoring' of https://github.com/snu-sf/llvm into assertsremain

commit 1acdbd67553ea4bc7c89e587b3d5fdf90374ef93
Author: petrosyh <yonghyun.kim@sf.snu.ac.kr>
Date:   Wed Jul 27 14:26:16 2016 +0900

    Apply clang-format

commit 8401d4f18817f67186ab0ce1a92db55ad7670f1a
Merge: 2cfbb64 473b1a3
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Wed Jul 27 14:22:33 2016 +0900

    Merge pull request #180 from naong606/refactoring
    
    Validate AndOrConst2

commit e4e42be06094d91cba03c327572ee4de9f95477e
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed Jul 27 14:01:08 2016 +0900

    Modify sub_add to deal with the case when x is a constant

commit 085b04b1408a1f2eefb7242965339b5e3faf3511
Author: petrosyh <yonghyun.kim@sf.snu.ac.kr>
Date:   Wed Jul 27 11:20:57 2016 +0900

    minor change

commit 473b1a383c7e20d57bf26258838435b598d466d3
Author: naong606 <naong606@naver.com>
Date:   Tue Jul 26 03:15:27 2016 +0900

    Validate AndOrConst2

commit acdcc988acdc3bfb035175b256aded23858ead69
Author: ailrun <jjc9310@gmail.com>
Date:   Fri Jul 22 18:02:18 2016 +0900

    Add InstCombine optimizations and_or_not
    
    Add and_or_not1
    ((~B) | A) & B = A & B
    
    Add and_or_not2 as comment
    B & ((~B) | A) = A & B

commit b678e787dc9ec023c16fea089cca79ea09e6fa11
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Jul 25 18:27:41 2016 +0900

    Add ConstantDataVector in LLVMBerry

commit 6664e662fe5d969c6db6cb4160f67f3ee2c266a4
Merge: addfac4 2cfbb64
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Sun Jul 24 22:12:52 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring
    
    Conflicts:
    	lib/LLVMBerry/ValidationUnit.cpp

commit addfac43d98e818b63eb12ec8d4eec5a1c6d59ac
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Sun Jul 24 22:11:37 2016 +0900

    Fix bugs of undef case in mem2reg

commit 2cfbb643c38a9db73f50aaa9772a7c83a2c07e01
Merge: 16dad14 8e9b029
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Sun Jul 24 18:23:28 2016 +0900

    Merge pull request #179 from snu-sf/revert-172-global_value
    
    Revert "Update to fit fixed global variable type in Vellvm"

commit 8e9b029ac0a7a8e7261ec1884c398e93befaf400
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Sat Jul 23 22:06:38 2016 +0900

    Revert "Update to fit fixed global variable type in Vellvm"

commit 16dad142d4cd23580231f754920c525360bb7ed4
Merge: 94ddcd7 5fb6abb
Author: aqjune <aqjune@gmail.com>
Date:   Thu Jul 21 18:21:35 2016 +0900

    Merge pull request #172 from alxest/global_value
    
    Update to fit fixed global variable type in Vellvm

commit 94ddcd7e6c03147603f5695b8c2390eaa0c16928
Merge: de5a342 4b8aba9
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Wed Jul 20 17:12:54 2016 +0900

    Merge pull request #171 from aqjune/whitelist
    
    Whitelist 구현 (#165)

commit 4b8aba91207c82cf4442dc78015b9d93b554fede
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed Jul 20 16:13:47 2016 +0900

    Move StartPass/EndPass position in InstCombine, add assertion in StartPass func

commit 7b9363676fd9037c3b142b9b4fd8f8bc9a65ec7e
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed Jul 20 01:45:29 2016 +0900

    Add include in Mem2Reg.cpp

commit 3f0c2153b1075cff84df19f7b2c8103268d8982b
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed Jul 20 00:01:04 2016 +0900

    Move mem2reg validation unit pass start/end pos

commit 9c8b90f377bcfd03444dc80fb6ae322483137e28
Merge: 03ba30b de5a342
Author: petrosyh <yonghyun.kim@sf.snu.ac.kr>
Date:   Tue Jul 19 14:53:25 2016 +0900

    Resolve conflicts

commit de5a3426225649ee9c1b74eaf76f03641e030baa
Merge: f34d7ff 9f7d17f
Author: Park, Sanghoon <sanghoon.park@sf.snu.ac.kr>
Date:   Tue Jul 19 14:35:18 2016 +0900

    Merge pull request #155 from aqjune/compares
    
    Compare 관련 instcombine 최적화

commit 5fb6abbbb510502c192ff6c50a50a615e2589db5
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Tue Jul 19 14:31:01 2016 +0900

    Update to fit fixed global variable type in Vellvm

commit 03ba30bc73fdd486a4e787ddaeebf5e9e04b6195
Author: petrosyh <yonghyun.kim@sf.snu.ac.kr>
Date:   Wed Jun 29 15:46:08 2016 +0900

    Fix sink instruction in multiple block case
    
    Try to fix sinking instruction multiple block case
    
    sinking_inst fixing...
    
    Fix header inclusion for sink
    
    Fix sink instruction in multiple block case
    
    put domtree to dictionary
    
    Toy example validation success
    
    fix without load sink
    
    Fixed sink for multiple block case

commit 75602b7dd88d9f79ae1d8d89353edff47563d1e7
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Tue Jul 19 11:05:10 2016 +0900

    Add pass boundary code for mem2reg, add opt arg impl, remove some debugging code

commit 45b3f05591905a2cdcbb02ab5dbeaab38a9b2383
Merge: ca5b49a f34d7ff
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Jul 18 19:11:16 2016 +0900

    Resolve conflicts

commit ca5b49a65111da2fb92f8215839e7b5c7b84cd0d
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Jul 18 19:10:38 2016 +0900

    Add pass-wise white list

commit f34d7ffa036ed690ef546f30b1e70e3d5afd40a1
Merge: df014d1 bad839a
Author: aqjune <aqjune@gmail.com>
Date:   Mon Jul 18 18:04:59 2016 +0900

    Merge pull request #167 from aqjune/addconstantexprs
    
    ATD에 instruction 추가, DCE 특수 케이스 및 Serialize 버그 수정

commit df014d1ad8fba753fb0b678d2e6a8948bf6e1fa1
Merge: 15a24d3 eb67995
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Mon Jul 18 17:59:59 2016 +0900

    Merge pull request #170 from alxest/gvn_pre_hard_
    
    Gvn pre hard

commit bad839a5a85e81b5b6973d01abc337b5c13b1bf8
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Jul 18 17:46:34 2016 +0900

    Remove trailing spaces and modify indentation

commit eb67995f4b723ffaf8d803b8fa8813984c1f3d2e
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Mon Jul 11 22:18:01 2016 +0900

    Fix segmentation fault, it also fixes many Validation fails

commit 0cd72caca56ab791ebb93a6aba8b13fbd7d0b34c
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Jul 18 17:44:29 2016 +0900

    Undo checking nullptr for I->getParent() in DCE, remove ValidationUnit::SetDefaultParent()

commit 8799aa090fc53e4369fb51c4ac70a896c1379353
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Jul 16 03:35:04 2016 +0900

    Remove PASS info in ValidationUnit, resolve the case(serialize() bug) issued by SanghoonPark

commit 2f7a3624fa525a0710a90b62cd54ac1fe211be4d
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Thu Jul 14 17:59:52 2016 +0900

    Resolve the case when I->getParent() == nullptr in DCE

commit 65851b3ac18a7c495a10f1e201bf2c40c962a6c7
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Thu Jul 14 15:30:54 2016 +0900

    Add select and casting instructions in ATD

commit 0306d488df2380136ba28f1049e87c5bf3f11fad
Merge: dd774da 15a24d3
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Jul 14 11:12:34 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring

commit dd774da97e804463f95b1be2c525f313f0a5a698
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Jul 14 11:10:47 2016 +0900

    Add undef case in mem2reg single store

commit 15a24d3f4c30106f5b829b684cff8dd9d52489f0
Merge: 92df3f4 1613a0d
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Wed Jul 13 19:01:53 2016 +0900

    Merge pull request #166 from aqjune/addconstantexprs
    
    Add inttoptr/ptrtoint constantexpr, add struct type

commit 1613a0d07db0190d78f2b96d5b62559a2e80dceb
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed Jul 13 14:14:45 2016 +0900

    Add inttoptr/ptrtoint constantexpr, add struct type

commit 07eb1bb2c890787a8fd70c05cadddac678f73a99
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue Jul 12 02:24:10 2016 +0900

    Bug fix in Mem2RegPHI

commit 634d2e611d449688978faafc6fd3b3ffb35ffddd
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sun Jul 10 22:57:38 2016 +0900

    Remove unused VI_evolvig_merged, also fix bug

commit e253f9ec1b7152612e0dc9931d5fa64592209bc7
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Mon Jun 20 18:02:32 2016 +0900

    Take changes from gvn_pre_hard

commit 73d92569f09af5fb61b9687e9ea4fd56792c7f67
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sun Jul 10 22:12:17 2016 +0900

    Cleanse some code

commit 5b7d4c2921fe2a3243e1487a9ff0373d5f8fa4c7
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Sun Jul 10 00:39:09 2016 +0900

    Fix assertion failure

commit f076ae825dab5b775e0ee67603e4042d9de8786b
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Jul 11 11:30:48 2016 +0900

    Add undef case

commit 92df3f4a6acc0c5ca4e16d60223c9342a0ac5dd9
Merge: 2140a2b 3645c327
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Mon Jul 11 10:26:16 2016 +0900

    Merge pull request #162 from alxest/debug_performance
    
    Replace printAsOperand with PrintLLVMName

commit 9f7d17f39bed2f1d6c4ab54da6f3335b4e9c4740
Merge: 1b94202 2140a2b
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Jul 9 06:42:42 2016 +0900

    Merge branch 'refactoring' of https://github.com/snu-sf/llvm into compares

commit 12bfbdbfbe6cdee814c4383eef8caa2f3713c911
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Jul 8 18:12:50 2016 +0900

    identical phi edge
    
    Conflicts:
    	lib/LLVMBerry/Hintgen.cpp

commit 2140a2b695031a95232d5e5a3544052d7fb801af
Merge: a436ada 528f55c
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Sat Jul 9 00:28:17 2016 +0900

    Merge pull request #160 from aqjune/loadload2
    
    Python 셋 에서 load-store 최적화가 내던 assertion failure 수정(2)

commit a436adaa68d577382132532ed76a0e6fd6a42d8f
Merge: d6a6625 f3cadc5
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Sat Jul 9 00:26:38 2016 +0900

    Merge pull request #156 from aqjune/dse
    
    Dead Store Elimination 구현 및 urem_zext/udiv_zext 파이썬 fail 해결

commit 45ffc39066309e6a31155359e6aae6954780f7a4
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Jul 8 18:05:42 2016 +0900

    Fix bug in isProperPHI function

commit 3645c327c3c27774757eb75e430741507cbf7839
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Fri Jul 8 13:53:32 2016 +0900

    Reflect code review

commit 8be79523ebd666a808c336f74a0317c855cf200f
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Fri Jul 8 11:11:03 2016 +0900

    Reflect code review

commit d6a6625e75c88bb49b5f080045c40bff00730fd2
Merge: d3a359e 402d655
Author: markshin <shin.dongyeon@gmail.com>
Date:   Thu Jul 7 15:02:04 2016 +0900

    Merge pull request #151 from aqjune/casts3
    
    Casting 관련 instcombine 최적화 4개

commit 499471648c57cc13bf18965abbc580178ae112d6
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Jul 7 12:43:48 2016 +0900

    Modify mem2reg to change tag Physical to Previous when move to other block

commit 8bd88f02634df9ff6db0413116e5ccef315b9f9a
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed Jul 6 22:04:48 2016 +0900

    Replace printAsOperand with PrintLLVMName

commit afe00559437b623f9461f491986f971fb74681a1
Merge: e45026c d3a359e
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Wed Jul 6 19:07:02 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring

commit e45026c8dc38d3c9aabbf38771d3cc5617178c72
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Wed Jul 6 19:06:00 2016 +0900

    Remove values Dictionary from mem2reg

commit d3a359e894d8f4c6b3d305ee44ccd47d70f666cd
Merge: 7c98a08 b0fb156
Author: Junyoung Clare Jang <jjc9310@gmail.com>
Date:   Wed Jul 6 17:04:01 2016 +0900

    Merge pull request #159 from alxest/cleanse_gitignore
    
    Cleanse .gitignore by @alxest (assigned by @ailrun)

commit 7c98a0814a86d72a20083a80110ab0dbebd5a8ed
Merge: 0c6cde8 6dc6bf8
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Wed Jul 6 16:51:06 2016 +0900

    Merge pull request #154 from aqjune/vector
    
    vector type을 atd에 추가

commit 6055b269f03b0622750fb740fbdb21c7561697ad
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue Jul 5 14:57:23 2016 +0900

    Update mem2reg Dictionary to support infrule

commit 528f55c057f69562776b53c8a24243e2228a67bd
Merge: c0ad4fe 0c6cde8
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Tue Jul 5 01:13:55 2016 +0900

    Merge branch 'refactoring' of https://github.com/snu-sf/llvm into loadload2

commit c0ad4fe05dee62fb10dcd8c16411686c4ffe63d1
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Tue Jul 5 01:13:42 2016 +0900

    Resolve the case when ptr1src/ptr2src in load_load opt are phi nodes

commit 1866c7ecb2ecc6f821adf785866465794574a4dc
Merge: 946171b 0c6cde8
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Jul 4 13:14:27 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring

commit 946171b3fc9b3a9906200599164c79187f1faf33
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Jul 4 13:13:57 2016 +0900

    Temporary commit

commit b0fb1566095b284314518878bd81f6d5058fea6b
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Mon Jul 4 12:39:37 2016 +0900

    Cleanse .gitignore

commit 0c6cde8373968c65a4801513af6a4b7e589ca3f7
Merge: 8972e64 5ea15b6
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Mon Jul 4 11:57:00 2016 +0900

    Merge pull request #157 from alxest/remove_submodule
    
    Remove cereal submodule

commit 5ea15b6d7e2675c8a935541c31637d6c7328ea64
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Mon Jul 4 11:34:42 2016 +0900

    Remove cereal submodule

commit 652dc1eb35da50167de37dd699b3521e3f8a7fc4
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Sun Jul 3 05:48:56 2016 +0900

    Add mem2reg Dictionary for tracing values

commit f3cadc591ee5e9c4afb5f192a5efe4382530427c
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sun Jul 3 02:31:07 2016 +0900

    Add const cases for UremZext, UdivZext

commit 1b94202a8a1e390e0364aac8763ea352e0890834
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Jul 2 16:01:22 2016 +0900

    Modify icmp ne case which incorrectly updated validationunit optimization name

commit 3e4d7ba60bf7817a835e317592a7d6ef005680ac
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Jul 2 03:10:35 2016 +0900

    Implement hint generation code for dead_store_elim, dead_store_elim2 optimizations

commit 2c5b9140fb275fbf2b66cc893670cf02f19e7911
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Jul 1 11:35:03 2016 +0900

    Temporary commit

commit 28e706273a8e0ea3401e26320e3f1841d0045865
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Thu Jun 30 20:40:56 2016 +0900

    Canonicalize add_associativity optimization

commit cd5ad4f4aa68b269315dcc82ddc0443dda00442e
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Thu Jun 30 20:39:40 2016 +0900

    Implement icmp-related instcombine optimizations

commit 8972e64eaea9be971c438effaa3b9b272d100409
Merge: 15c4ab9 28300c3
Author: aqjune <aqjune@gmail.com>
Date:   Wed Jun 29 17:46:47 2016 +0900

    Merge pull request #152 from aqjune/loadload2
    
    Python 셋 에서 load-store 최적화가 내던 에러 수정

commit 6dc6bf85060a08ec1fc927188006920f571cf722
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed Jun 29 16:32:47 2016 +0900

    Add vector type

commit 311537e0a1d31c9c8d2fd75f58a68c04ac540307
Merge: 2f0462d 15c4ab9
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Jun 27 13:59:10 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring
    
    Conflicts:
    	lib/LLVMBerry/Hintgen.cpp
    	lib/LLVMBerry/Structure.cpp
    	lib/Transforms/Utils/PromoteMemoryToRegister.cpp

commit 28300c31fa903cccd6694b7a78161ef9eecf092f
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sun Jun 26 14:52:05 2016 +0900

    Erase empty line

commit 6c9b4a19bc49581317316664f3c79928c639c6b0
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sun Jun 26 14:50:37 2016 +0900

    Uncomment commit() part

commit 4fe9656ff5e6f5eff6cac7794bdfa510722c7711
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Jun 25 02:28:11 2016 +0900

    Modify loadload opt to correctly propagate diff block invariant

commit 30e7834c8ff3030fe58f63436aa40dda0085fd3e
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Jun 25 02:17:10 2016 +0900

    Resolve unexpected runtime error

commit d55a9f32e3b4483e9d13434e887289b100ecf6a3
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Jun 25 01:56:26 2016 +0900

    Resolve the case when AliasAnalysis::getModRefInfo calls stripPointerCasts() in load-load opt

commit c257bd9aeb35129841ef901793671e61f764241c
Merge: 7eeda56 15c4ab9
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Jun 25 00:58:20 2016 +0900

    Resolve conflicts

commit 7eeda565260c2db6912e5cd72766f434baeebbf8
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Jun 25 00:57:26 2016 +0900

    Add instructions fpext, fptrunc to LLVMBerry hint format

commit 15c4ab9eaaa75cad70ffb0316e2d4d4fd8e2296f
Merge: eb65f4e 2c3a739
Author: aqjune <aqjune@gmail.com>
Date:   Sat Jun 25 00:55:01 2016 +0900

    Merge pull request #145 from SanghoonPark/mem2reg_fix
    
    Prevent accessing null in mem2reg

commit c9a0e954a1687af9550ab72d8d0f075eca1e2072
Merge: 657d454 eb65f4e
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri Jun 24 17:15:45 2016 +0900

    Merge branch 'refactoring' of https://github.com/snu-sf/llvm into loadload2

commit 657d4548d8a18dbb107e5cd87d07e45458d5e233
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri Jun 24 17:15:37 2016 +0900

    Print what instruction/type it is if failed to make

commit 402d655cdeb689d98a3c3b55e034fef619053b36
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri Jun 24 15:40:50 2016 +0900

    Apply clang-format

commit 2fa6a919eef1178b08d08848b85c862260638dcf
Merge: b44a647 eb65f4e
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri Jun 24 15:12:07 2016 +0900

    Merge with snu-sf refactoring

commit b44a6475f02184a4c6149d796116e5eadd8540ef
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri Jun 24 14:11:27 2016 +0900

    Implement 4 micro optimizations (trunc_onebit, zext_xor, zext_trunc_and_xor,
    zext_trunc_and)

commit 2f0462d3870ae7f820114cf5132b17772fa1032f
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Jun 23 19:28:29 2016 +0900

    Modify mem2reg general validation to cover more cases

commit 582deff849588db44f76a2a7278147db6ab97b9f
Merge: 7a5e389 eb65f4e
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Jun 23 04:32:50 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring
    
    Conflicts:
    	include/llvm/LLVMBerry/Hintgen.h
    	include/llvm/LLVMBerry/Infrules.h
    	include/llvm/LLVMBerry/Structure.h
    	lib/LLVMBerry/Dictionary.cpp
    	lib/LLVMBerry/Hintgen.cpp
    	lib/LLVMBerry/Infrules.cpp
    	lib/LLVMBerry/Structure.cpp

commit 7a5e3896a59dfc89465089990397dbbb8ae6340d
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Jun 23 04:07:26 2016 +0900

    Apply clang-format

commit dbef7484632bcdc68e5a0f77cb926142a1553127
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu Jun 23 04:03:41 2016 +0900

    Add hintgen function for PHI

commit 2c3a7395781b902c0ba4af20dc99be45dfc101e8
Merge: 2c79558 eb65f4e
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Wed Jun 22 17:24:22 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into mem2reg_fix
    
    Conflicts:
    	include/llvm/LLVMBerry/Structure.h
    	lib/LLVMBerry/Hintgen.cpp
    	lib/LLVMBerry/Structure.cpp

commit eb65f4e7de5a8b7f5f31466fa3770547dd3db798
Merge: 459e853 7397786
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Wed Jun 22 17:18:10 2016 +0900

    Merge pull request #148 from alxest/clang-format
    
    Apply clang-format

commit 7397786625332a98b62d4e4af38de50db27c7fb0
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed Jun 22 12:26:22 2016 +0900

    Apply clang-format

commit 459e853f34549a1ab73921d48a171b0079a1b622
Merge: 428b6d8 c98f3fb
Author: YoungJu Song <youngju.song@sf.snu.ac.kr>
Date:   Wed Jun 22 12:14:33 2016 +0900

    Merge pull request #147 from aqjune/loadload2
    
    Load-load / Load-store 최적화 에러 수정

commit 04f52d41aad3280b834a83a171f1a800fdab400a
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Wed Jun 22 00:25:23 2016 +0900

    Initial success of mem2reg_general validation with PHI

commit 2c795587d3466acf0aa5606b2b158736d5820c7b
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue Jun 21 19:24:10 2016 +0900

    Remove makeExpr_fromStoreInst (use TyExpr::make)

commit c98f3fbae6736e5528e6f24394d53cf767d73538
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Tue Jun 21 15:25:01 2016 +0900

    Implement void type, constexpr bitcast, function type, etc

commit 553fa793a00f34b3301e22f0940d7f0c451331f4
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Jun 20 19:01:30 2016 +0900

    Resolve compilation errors

commit 7a1155dbea2acef7eea2aa3f190e8e272144692b
Merge: afe9a2e 428b6d8
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Jun 20 18:56:33 2016 +0900

    Resolve conflicts

commit afe9a2e50d3e51b4e17f94f9e29ea41adc35cdaa
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Jun 20 18:50:53 2016 +0900

    Passes test cases in inputs_full'

commit 99a9b57d29126db148332e75de4d8dc66f648095
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Jun 20 16:15:46 2016 +0900

    Prevent accessing null in mem2reg

commit e57c3c9aa04b284a43b7e1cb9886c0f11db548f1
Merge: 1cd8d4a 428b6d8
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Jun 20 04:04:13 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring

commit 1cd8d4a3f1cdd8eb5a1003c984328d74e4cf156c
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon Jun 20 04:03:35 2016 +0900

    Modify mem2reg validation to cover more cases

commit 726f6377e2db7a2bb4c0cde6d2883515b97cfb17
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Sun Jun 19 05:35:48 2016 +0900

    Modify mem2reg general case to search successors

commit f6cff165727a96d82d6a5cf0888e6866e4d40abd
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Jun 18 22:31:58 2016 +0900

    Add InstCombinePHI.cpp modification

commit e2fded70ed28097c21177e9f08caac2f2a782064
Merge: f0527c1 1715fc6
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Jun 18 22:28:15 2016 +0900

    Merge commit '1715fc6e895a45082cc06b6fe4bc91b14f4f123f' into refactoring

commit f0527c162baa176ab69d35a974f69388c55eca8e
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Jun 18 22:28:12 2016 +0900

    Commit two more modified files

commit 6e3ab038caaf3ac98ed3033d05ce4e86e3b2bf41
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat Jun 18 22:27:35 2016 +0900

    Commit before merging alxest/gvn_pre_hard 1715fc6e895a45082cc06b6fe4bc91b14f4f123f

commit 428b6d80f3366d45996a22f4367db643f542b37f
Merge: f055465 bba666d
Author: Jeehoon Kang <jeehoon.kang@sf.snu.ac.kr>
Date:   Fri Jun 17 17:36:15 2016 +0900

    Merge pull request #142 from aqjune/casts2
    
    cast-cast 쌍에 대한 Instcombine 최적화 검증 2

commit 767cb28e65e8da8955fb1517ed5ae9899c268fdc
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Jun 17 16:53:31 2016 +0900

    Initial success for mem2reg general case

commit 0089fca8a5d398ab00e8fc555be009a83e9f6757
Merge: 3522d18 f055465
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Jun 17 15:16:26 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring

commit 3522d183fd9802b934908b9e64e0bb8b7d2a1baf
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Jun 17 15:15:51 2016 +0900

    Update Dictionary for mem2reg

commit bba666dcb9d4f3b73ab589c65e4506d3179263a7
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri Jun 17 13:16:22 2016 +0900

    Use getOpcodeName() in identifying inference rule

commit f0554657e2a48f952a7f4b4b78455510d76ef6a0
Merge: c8b5ac0 822c3e4
Author: Jeehoon Kang <jeehoon.kang@sf.snu.ac.kr>
Date:   Fri Jun 17 12:49:16 2016 +0900

    Merge pull request #144 from SanghoonPark/refactoring
    
    Fix bug in mem2reg

commit c8b5ac0318afe31da5e013e2e896c57f38b80b27
Merge: bbc98e6 0544d1e
Author: Jeehoon Kang <jeehoon.kang@sf.snu.ac.kr>
Date:   Fri Jun 17 12:46:23 2016 +0900

    Merge pull request #143 from markshin/refactoring_foldPHIbin
    
    python test fail case fix

commit 822c3e4927211cee26a1bc518d7c65f52a0b2596
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri Jun 17 02:56:52 2016 +0900

    Fix bug in mem2reg

commit 0544d1e7fb1c29686c77ff6340efb70de9f4084f
Author: markshin <shin.dongyeon@gmail.com>
Date:   Thu Jun 16 14:58:32 2016 +0900

    indentation changed

commit 4936c362b217b21b78b2c70fbb017a5266f0237c
Author: markshin <shin.dongyeon@gmail.com>
Date:   Thu Jun 16 14:54:58 2016 +0900

    after python test bug fixed

commit 7c75c9872815b6f69047d6bdec797cd54bea592c
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Thu Jun 16 14:49:05 2016 +0900

    Implement cast-cast elimination 2

commit 1715fc6e895a45082cc06b6fe4bc91b14f4f123f
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Tue Jun 7 17:32:09 2016 +0900

    Add substitute_rev rule

commit cdb2ccc1a99aa3bac0b7b61bba0c1b3a8bc5bddc
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Tue Jun 7 15:48:39 2016 +0900

    WIP, add some infrule/propagate
    
    should swap lessdef direction

commit 2dd939641b086cc6b3ba7382006483cf2be344a6
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Tue Jun 7 02:22:24 2016 +0900

    Use substitute infrule instead of replace_rhs

commit 9c0c70fbb07bed12d2a03a8af3bd03267e15ff0e
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Tue Jun 7 02:14:41 2016 +0900

    Add substitute rule

commit 042825f94f4c416672bfebfde37e4b316fdc35d8
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Tue Jun 7 01:13:59 2016 +0900

    PRE hard case - working on replace_rhs

commit e901d476be6e90b455efa776713904ea8def6743
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Mon Jun 6 00:39:54 2016 +0900

    Let separate RHS(VI) != RHS(CurInst) case in GVN_PRE

commit fecb3d3765361f7b94060beb5a17e29f02929bb4
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Mon Jun 6 00:39:39 2016 +0900

    Add appendToDescription

commit 289e34c9d27412a78fe1e49bf97ee14c9e8bc528
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Thu Jun 2 18:05:57 2016 +0900

    Add validation hint for simple case of PRE

commit bbc98e6c766cfb4be724981309d1f4ab79bf6cf2
Merge: 2b5f095 4996699
Author: aqjune <aqjune@gmail.com>
Date:   Tue May 31 14:03:03 2016 +0900

    Merge pull request #139 from aqjune/xorsimplify
    
    xor 연산에 대한 Instruction Simplify 4가지 최적화

commit 499669904fac47fa493d85cd6639118e921d5493
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon May 30 15:36:09 2016 +0900

    Resolve conflicts

commit 6ffe621530e3603f0a35f8a26ad8abb2df7e82b9
Merge: eb576fe 2b5f095
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon May 30 13:04:14 2016 +0900

    Resolve conflicts

commit 2b5f095a9ca761b29a4b8fa89506ac95a31030dc
Merge: c59f1d3 368b905
Author: Jeehoon Kang <jeehoon.kang@sf.snu.ac.kr>
Date:   Fri May 27 14:30:12 2016 +0900

    Merge pull request #133 from aqjune/casts
    
    12가지의 cast-cast 쌍에 대한 Instcombine 최적화 검증

commit 368b90543764ee95f0852e6362f1576787af6b59
Merge: 503cb46 c59f1d3
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri May 27 14:21:39 2016 +0900

    Resolve conflict with snu-sf refactoring

commit c59f1d32ec4111e8ffcefd2d38f5a6e5e8c4fbcd
Merge: c436fc2 818a338
Author: Jeehoon Kang <jeehoon.kang@sf.snu.ac.kr>
Date:   Fri May 27 11:46:19 2016 +0900

    Merge pull request #130 from SanghoonPark/refactoring
    
    Mem2Reg: 모든 register promotion을 한번에 검산 / SingleStore, SingleBlock 검산

commit 818a33802558e36be4f45c2f3632e1396aa4abcf
Merge: 252789d c436fc2
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Thu May 26 10:03:59 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring

commit c436fc27903170e23b17078fa2d851cd2d248057
Merge: 5ba2d95 888ddc9
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Wed May 25 15:35:36 2016 +0900

    Merge pull request #138 from alxest/dead_block
    
    Dead block

commit eb576fe4aa1b4cdbc6e3c2e019a2d61b19fc9d17
Merge: 77891a7 5ba2d95
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed May 25 00:39:42 2016 +0900

    Merge branch 'refactoring' of https://github.com/snu-sf/llvm into xorsimplify

commit 77891a72a079eabbdc18af0245408711fcf96ef3
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed May 25 00:30:26 2016 +0900

    Add hint generation codes for xor_not / xor_undef / xor_same / xor_zero

commit 252789d1a49d5bb7cd2794f68e62fcc948f61e54
Merge: 629722d 5ba2d95
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue May 24 15:05:37 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring

commit 629722dac82979c4e6cc85091ba00c07ac93fdb2
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue May 24 15:05:17 2016 +0900

    Merge Dictionaries for Mem2Reg

commit 2c013d374e634daf8485f87284f9de13d798d6bd
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue May 24 13:05:37 2016 +0900

    Make repeated codes to function in PromoteMemoryToRegister.cpp

commit efe6af4f7730c1c704f33105e2216928d81d72ff
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Tue May 24 12:59:27 2016 +0900

    Add hintgen function for propagate noalias, store, and load

commit 888ddc91ac4373a7c2e8ba68b92fd3ec4aff4791
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Mon May 23 22:28:12 2016 +0900

    Remove description used for debugging

commit 6ab7f83c019725318f5a9c62e3ba30b4f1c0f047
Merge: 6d70200 5ba2d95
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Mon May 23 22:03:50 2016 +0900

    Merge branch 'refactoring' into dead_block
    
    * refactoring: (25 commits)
      Modify return code
      Add ofstream fail check in ValidationUnit::commit
      Remove leader_value and more cleanups
      Support commutativity in GVN
      Remove temporary assert
      Add [icmp eq A B = true => A = B] and [icmp ne A B = false => A = B].
      Add asserts in GVN hint generation, and add explanation for copied function in Hintgen.cpp
      Remove unnecessary comments
      Use macro in generating hints of GVN
      Add predicate inversion case in GVN
      Change dictionary usage at finding leaderBB in GVN
      Add branch case of GVN
      WIP - cleanup GVN hint
      Refactor GVN hint- add simple case
      Commit for moving to aqjune llvm repo
      Remove unused variables in Hintgen.cpp
      Remove nested intrude
      Split GVN case from generateHintForTrivialDCE
      Add "appendAdmittedToDescription"
      Add DCE hint generation for GVN
      ...
    
    Conflicts:
    	include/llvm/LLVMBerry/Hintgen.h
    	include/llvm/LLVMBerry/Infrules.h
    	lib/LLVMBerry/Hintgen.cpp
    	lib/LLVMBerry/Infrules.cpp

commit 6d70200e68c30f36b983faf12d95e54ebca2cade
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Mon May 23 19:49:28 2016 +0900

    Update hint generation for dead_block_remove, violating intrude design
    
    It uses "myHint" global variable, modifying original llvm code

commit 9d99d48ec77709a25f15187e228a71ff375bfe68
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Mon May 23 19:47:39 2016 +0900

    Add implies_false infrule

commit 5ba2d957199ad8f3aba51588775c2895e53ffd12
Merge: 4a26ca1 c84dc8a
Author: aqjune <aqjune@gmail.com>
Date:   Mon May 23 16:21:20 2016 +0900

    Merge pull request #136 from aqjune/commitfailcheck
    
    ValidationUnit::commit 에서 ofstream fail check

commit 4a26ca1a63e43774dc1e35c326aa66dd37b9dad5
Merge: 0b7f695 ed45652
Author: Jeehoon Kang <jeehoon.kang@sf.snu.ac.kr>
Date:   Sun May 22 22:37:56 2016 +0900

    Merge pull request #134 from kim-yoonseung/refactoring_gvn_eq_more
    
    GVN eq, neq case 추가

commit c84dc8a012ca9147d25ee5f2a0205545d97c105b
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Thu May 19 20:56:53 2016 +0900

    Modify return code

commit b9792c709b1aead5434bbf3663dd4a39966a1791
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Thu May 19 20:47:07 2016 +0900

    Add ofstream fail check in ValidationUnit::commit

commit ed45652f5123c5e2dd1a6a69acd767e4cc451df4
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Thu May 19 16:45:04 2016 +0900

    Remove leader_value and more cleanups

commit a3656fa69387ebb8ef0741df0b8a564397d3952c
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon May 16 12:34:07 2016 +0900

    Dictionary: integrate StoreVal, StoreExpr, and StoreOp0 to StoreItem

commit 575267af25fbaa1ab5258eef611ea11682000b2a
Merge: dc0e592 0b7f695
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon May 16 00:54:17 2016 +0900

    Merge remote-tracking branch 'upstream/refactoring' into refactoring

commit dc0e5924bb2c83789e15de063ae0540b1f3d16ce
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon May 16 00:53:42 2016 +0900

    Add comments in Dictionary.h, PromoteMemoryToRegister.cpp

commit 68f8cc7efcf01566c849d20cee8e0660b13c0e35
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Mon May 16 00:22:16 2016 +0900

    Change initialize and remove redundant spaces in Dictionary

commit 503cb461a743791cd6d08702006a334511e82268
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sun May 15 17:37:00 2016 +0900

    Implement 12 cast-cast instcombine optimizations

commit bd97430520f731e961dd22ec71174bf7ef7a9601
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Sat May 14 22:46:11 2016 +0900

    Fix bugs and add comments in PromoteMemoryToRegister.cpp

commit 559a517fccfa528feda2c2728b9dcf4aa4ef489b
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Sat May 14 22:45:35 2016 +0900

    Add comment and change variable name in TyPosition::make

commit b14532b7d5377381dcb054d096961ee8f0f8443b
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Sat May 14 22:44:25 2016 +0900

    Add macros in Hintgen.h

commit e7aedfe226f8f0c657914514baf75047915dcfb0
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu May 12 22:35:19 2016 +0900

    Add false_encoding insertion in dead block

commit cfc7b8a68e5c0eff596cd7af0d782c20283adbbd
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Thu May 12 20:33:45 2016 +0900

    Add nop_insertion, with ex_falso, passes 5 more tests

commit 2fc7452e1b83ea017e2e8dd8a7d01d881d02f17b
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed May 11 20:43:44 2016 +0900

    Move VU begin, not to fail because of nested VU

commit 420942cc35aa801cb94eab3121526c7af4bba35f
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed May 11 18:55:41 2016 +0900

    Add hint generation for dead_block_remove

commit 3da3212a9fa45f477f619e2c35baef1e08a531de
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Wed May 11 17:19:15 2016 +0900

    Support commutativity in GVN

commit 0b7f69518ae45d2b4a517fa387708d2dd6f6b40a
Merge: 29da853 5cbe649
Author: aqjune <aqjune@gmail.com>
Date:   Wed May 11 15:03:19 2016 +0900

    Merge pull request #121 from SungMinCho/refactoring
    
    get block name and variable name when it has "". and aboud dce fails....

commit 5cbe649047fcb3b4e4b8600cbd5f10047ed488dd
Merge: e049226 f7c0856
Author: SungMinCho <tjdals4565@gmail.com>
Date:   Wed May 11 09:38:32 2016 +0900

    Merge pull request #3 from aqjune/smcdce
    
    snu-sf/refactoring과의 conflict 해결 (2)

commit f7c0856b371657e9bfd47d5bc277b46782b024a2
Merge: 30e5a8a 29da853
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed May 11 01:08:05 2016 +0900

    Resolve conflicts

commit 30e5a8ac4a05b610789cb51eb80b3e64f0d0f532
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed May 11 01:06:18 2016 +0900

    Remove temporary assert

commit caa58f705848dc51856657f619025b93d8523f1d
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue May 10 18:22:20 2016 +0900

    Add [icmp eq A B = true => A = B] and [icmp ne A B = false => A = B].

commit 29da85376762a13eab710fed951ebef62e9dafb7
Merge: 949ba6b 67e5ea8
Author: Jeehoon Kang <jeehoon.kang@sf.snu.ac.kr>
Date:   Tue May 10 15:20:55 2016 +0900

    Merge pull request #129 from kim-yoonseung/refactoring_gvn_refact
    
    GVN refactoring & icmp_inverse 케이스 추가

commit 67e5ea8b596d4ead5c91994203e2b90900a370fc
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue May 10 14:24:19 2016 +0900

    Add asserts in GVN hint generation, and add explanation for copied function in Hintgen.cpp

commit 7a1042e540193727447f56f07704f3d08b3dc802
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue May 3 18:05:14 2016 +0900

    Remove unnecessary comments

commit 6af6bbb5a03e20e95368eb7d07d0e42f1b8412ae
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue May 3 17:47:23 2016 +0900

    Use macro in generating hints of GVN

commit 683b430e1d6204435fe400fa12683a963d32072b
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue May 3 16:32:58 2016 +0900

    Add predicate inversion case in GVN

commit ca97e2dc414b344b362cf4010d5bc01d9ff17d26
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue May 3 14:06:57 2016 +0900

    Change dictionary usage at finding leaderBB in GVN

commit a888cba7a3c365a445edee2335192c8e32cab504
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Mon May 2 10:52:44 2016 +0900

    Add branch case of GVN

commit 0ad2731f7a605af6a83946fb6561ac21e41837b1
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Wed Apr 27 17:28:47 2016 +0900

    WIP - cleanup GVN hint

commit 1b4cd3c36f6e8bec2cf363a7a9fc13e2e4820918
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue Apr 26 17:44:53 2016 +0900

    Refactor GVN hint- add simple case

commit 949ba6b673155db5abcdaa57ba194099d048d865
Merge: 97f2a07 5de74ea
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue May 10 13:37:25 2016 +0900

    Merge pull request #132 from kim-yoonseung/refactoring_null
    
    Add null case in TyConstant::make.

commit 5de74ea71f6537a90027f54417af8d25077470e3
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue May 10 13:35:24 2016 +0900

    Add null case in TyConstant::make.

commit 97f2a0760b8bb7ae48c819948ad784fa10472190
Merge: 3ff3807 b50ecc9
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Tue May 10 13:31:44 2016 +0900

    Merge pull request #131 from kim-yoonseung/refactoring_null
    
    Constant에 Null 추가

commit e0492261539171e72aa004a5b860d725d4aadda6
Merge: 494cba4 38e6a5b
Author: SungMinCho <tjdals4565@gmail.com>
Date:   Tue May 10 12:11:16 2016 +0900

    Merge pull request #2 from aqjune/smcdce
    
    snu-sf/refactoring과의 conflict 해결

commit b50ecc9232a7088c0e8368333aa5a04163b00589
Author: Yoonseung Kim <yoonseung.kim@sf.snu.ac.kr>
Date:   Mon May 9 18:02:32 2016 +0900

    Add ConstNull.

commit 38e6a5b29032ea06d9114390943cf8e062cb4030
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sat May 7 23:30:58 2016 +0900

    Commit for moving to aqjune llvm repo

commit 65a2d62af690be70039b1e98373e07691ed997d7
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Sat May 7 01:23:47 2016 +0900

    Mem2Reg: fix bug in finding nearest store of load

commit 0cc253e160f062b62999c6a133c18fe0fa76d880
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri May 6 14:47:57 2016 +0900

    Modify Mem2Reg validation: validate promotion of all allocas at once (single store/block)

commit 3d9718b16defdf8c64326b7f8dd7098737905bd3
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri May 6 14:47:15 2016 +0900

    Add dictionaries for Mem2Reg

commit 0886346d409a141976cd5b8a963b0c841eb99493
Author: Sanghoon Park <sanghoon.park@sf.snu.ac.kr>
Date:   Fri May 6 14:46:24 2016 +0900

    Add TyPosition::make with index argument

commit 5e2e690a79e87b63887a18b1ec9a8f47ff14f482
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed May 4 11:51:37 2016 +0900

    Remove unused variables in Hintgen.cpp

commit c7351cc3658e429860fd7242a2ae749ed1b79305
Merge: 494cba4 3ff3807
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed May 4 11:47:56 2016 +0900

    Resolve conflicts

commit 3ff3807c4f45add8b901c515011fd6869461d6c4
Merge: e453482 5cd78ee
Author: aqjune <aqjune@gmail.com>
Date:   Tue May 3 15:03:13 2016 +0900

    Merge pull request #128 from aqjune/andor
    
    [Urgent] Hotfix compilation error

commit 5cd78eebaa6ca2b2de249cc1d6f986ab1cf5efbf
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Tue May 3 15:02:03 2016 +0900

    Hotfix compilation error

commit e453482d7820efeb672272fe9f41e482a270ebbf
Merge: b622a4b d50c0df
Author: Jeehoon Kang <jeehoon.kang@sf.snu.ac.kr>
Date:   Tue May 3 12:23:54 2016 +0900

    Merge pull request #127 from aqjune/andor
    
    InstSimplify 6개 및 InstCombine 3개 구현, 일부 hintgen 코드 매크로화

commit d50c0dfa17b74a3e519711fb2d4d103515222aae
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon May 2 23:14:08 2016 +0900

    Implement 6 or simplifies and 3 or optimizations

commit 0ed227cbb534e66c387a649d008a22c53329510f
Merge: 128f2f2 b622a4b
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon May 2 16:54:28 2016 +0900

    Resolve conflicts (hintgen.h, structure.h)

commit 128f2f27df8775dca9d304eee451e81de85c53e3
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon May 2 16:53:33 2016 +0900

    Add some macros

commit b622a4b128e306150169fafd251dbd35817b11f5
Merge: a91787d bfc9084
Author: aqjune <aqjune@gmail.com>
Date:   Mon May 2 16:51:10 2016 +0900

    Merge pull request #123 from aqjune/loadload
    
    Load-load / Load-store 최적화 구현

commit bfc908420c06f01b8c634375fcd53e22475ae7cc
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon May 2 16:50:07 2016 +0900

    Add more macros in Hintgen.h

commit fc8cc9bd4cf95d57d2b0987728bb146545df8fc4
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon May 2 16:49:40 2016 +0900

    Simplify hint gen codes in InstCombineLoadStoreAlloca.cpp, and add comments to DictKeys in Dictionary.h

commit 42f134f648c3766cbd7253bf1c8c3c64da0d0036
Merge: 7aa79d7 a91787d
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon May 2 15:37:34 2016 +0900

    Merge branch 'refactoring' of https://github.com/snu-sf/llvm into andor

commit 975f739ca1c1d2e3724b6f47653f3acd15c509e5
Merge: c3905bb a91787d
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon May 2 14:31:45 2016 +0900

    Merge with master

commit a91787de22207bf503a155fd7e20704fd7d88ff0
Merge: 629907f 7af9ca0
Author: aqjune <aqjune@gmail.com>
Date:   Mon May 2 13:16:49 2016 +0900

    Merge pull request #126 from markshin/refactoring_foldPHIbin
    
    changed func name

commit 7af9ca0f57b020b8c2f644bf11ab8b9eb94f7dc8
Author: markshin <shin.dongyeon@gmail.com>
Date:   Mon May 2 13:15:09 2016 +0900

    changed missing indent part

commit 81850537d3d967206d0eca8418e03781b58a21d0
Author: markshin <shin.dongyeon@gmail.com>
Date:   Mon May 2 13:01:41 2016 +0900

    change capital to lower case

commit 1df974a3d52a3b65d53ec20298a14b9f0c75c43f
Merge: e0aea0a f82c416
Author: markshin <shin.dongyeon@gmail.com>
Date:   Mon May 2 11:59:01 2016 +0900

    fix conflict

commit e0aea0aa75152b3ff0b93989521e2a18f595253d
Author: markshin <shin.dongyeon@gmail.com>
Date:   Mon May 2 11:55:50 2016 +0900

    change getPredicate func name

commit 7aa79d7de0275a640f0e97ecab0917ae596d84e4
Merge: 855c82ea 629907f
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon May 2 11:10:59 2016 +0900

    Implement InstSimplify for OR, and merge with snu-sf

commit 629907f25aa70f0cab5d64a551607fcf59643441
Merge: 2894fe1 f82c416
Author: aqjune <aqjune@gmail.com>
Date:   Mon May 2 11:03:27 2016 +0900

    Merge pull request #125 from markshin/refactoring_foldPHIbin
    
    Refactoring fold_phi_icmp

commit f82c416ce05a073ae2dc023f7026249180f65574
Author: aqjune <aqjune@gmail.com>
Date:   Mon May 2 10:59:34 2016 +0900

    Update Structure.cpp

commit 6cffea9e88929e048610c5d2a31ebdc2c0a840c9
Author: markshin <shin.dongyeon@gmail.com>
Date:   Mon May 2 10:52:40 2016 +0900

    changed missing indent part

commit 8f3a652caea2c027d5733c1da3cae5b84eac59e4
Author: markshin <shin.dongyeon@gmail.com>
Date:   Mon May 2 10:11:32 2016 +0900

    fix indent again

commit cc55447e346c38de32a42fa9dfe90f53afe00b0f
Author: markshin <shin.dongyeon@gmail.com>
Date:   Mon May 2 10:07:26 2016 +0900

    change indent

commit 855c82eaaed55662ce8aa4ea0a00605f345a7ed6
Merge: c078e43 c3905bb
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sun May 1 22:26:45 2016 +0900

    Merge with load-load branch

commit c078e435a8bf526093523e98b979810274f932f7
Merge: 5b0f5b5 c1a69f8
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sun May 1 22:24:44 2016 +0900

    Merge remote-tracking branch 'origin/loadload' into andor

commit 5b0f5b519b56e7474c38b3cf0c4f187dd216e00b
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sun May 1 22:23:45 2016 +0900

    Commit update for 6 OR simplifications

commit 773c4984e8d069c2d4d7b4c47ef33cf9459bd11f
Author: markshin <shin.dongyeon@gmail.com>
Date:   Sun May 1 16:35:18 2016 +0900

    changed TyFcond

commit 86cbc29e005e320a6839aee158e3940f830f5cc9
Author: markshin <shin.dongyeon@gmail.com>
Date:   Sun May 1 16:32:37 2016 +0900

    finished in icmp case

commit 7371b8656709fa3691249e337611145747e6e6a3
Author: petrosyh <yonghyun.kim@anakin.sflab.local>
Date:   Sun May 1 12:53:30 2016 +0900

    icmp validation succeeded but fcmp

commit c3905bb067309d394f359443758f55977c50faee
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri Apr 29 17:59:24 2016 +0900

    Correctify wrongly modified part in InstCombineMulDivRem.cpp

commit b6d881aaa131d11cd333ff0a237bf0cb27b134e5
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri Apr 29 17:57:30 2016 +0900

    Modify source code to use enhanced dictionary type

commit 3d3c67c02d0b4e457414960a33c467e0b803d066
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Apr 29 17:32:04 2016 +0900

    change prediate type and split icmp and fcmp

commit 75b13ff390016c626efe13279919ea62a737480b
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Apr 29 16:11:28 2016 +0900

    cmp inst in constant case

commit 4648c4816dfb5cd4986a72a0519c819d17fc4ab6
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Apr 29 12:48:52 2016 +0900

    cmp inst done in id case

commit 91e05f34c74c65007cd61882c8b7f35eeceeb5ec
Author: markshin <shin.dongyeon@gmail.com>
Date:   Thu Apr 28 14:30:44 2016 +0900

    made ICmpInsn

commit 494cba411569b4bdc84ff20987450b83ba6319d6
Merge: 11671e8 eb04d28
Author: SungMinCho <tjdals4565@gmail.com>
Date:   Thu Apr 28 14:19:06 2016 +0900

    Merge pull request #1 from alxest/dce_bug_fix
    
    merge with alxest's dce

commit eb04d28c0be10b8fd441ea824f9a344642f9e829
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed Apr 27 23:32:11 2016 +0900

    Remove nested intrude

commit 769700c41c4750a55ed51b956177e40eb09b31ff
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed Apr 27 23:10:58 2016 +0900

    Split GVN case from generateHintForTrivialDCE
    
    Those two admits have different reasons and it should be separated

commit 778d6777b079bb81693d7f158a3eeccd835739ce
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed Apr 27 22:24:53 2016 +0900

    Add "appendAdmittedToDescription"

commit 58b8da81b23948c57f3ed19c466813be11ebac6c
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Wed Apr 27 22:17:03 2016 +0900

    Add DCE hint generation for GVN

commit c1a69f8b1f086d236eae966f3ff791f9f6fd2a31
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed Apr 27 15:26:34 2016 +0900

    Remove redundant line in lib/IR/CMakeLists.txt

commit 5921c9e9a139ea156138e3827548cfc53f6c348d
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Wed Apr 27 15:03:58 2016 +0900

    Load-load / Load-store initial version

commit c62ca36e7b9d8f932c604d84eeae4a7516d060b5
Author: alxest <youngju.song@sf.snu.ac.kr>
Date:   Tue Apr 26 22:52:15 2016 +0900

    Remove redundant code

commit 11671e8199176a8f63a4ccd2fe5876aff288235a
Author: SungMinCho <tjdals4565@gmail.com>
Date:   Tue Apr 26 21:37:28 2016 +0900

    Set description at other dce places

commit 318e8eb011ef1d1f1fa6eb024df580c08ec59b0c
Author: SungMinCho <tjdals4565@gmail.com>
Date:   Tue Apr 26 14:49:28 2016 +0900

    Get variable name when it has "

commit 241d78022c4ef5ddc6fdde16099c5e1c5674d1ab
Author: SungMinCho <tjdals4565@gmail.com>
Date:   Tue Apr 26 13:28:18 2016 +0900

    Add description when dce call inst

commit 09e9164b52c58dde415dc706ba50fafd8f76995b
Author: SungMinCho <tjdals4565@gmail.com>
Date:   Tue Apr 26 01:16:19 2016 +0900

    Get block name in smarter way

commit 7aec020082d6e00aa894b3a6d89c8aea806430d5
Merge: 68afd3a 2894fe1
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Apr 25 15:32:04 2016 +0900

    Resolve conflict with master

commit 2894fe1050495b7772cb5879f26a7df43539b3fb
Merge: 6925624 a9775eb
Author: aqjune <aqjune@gmail.com>
Date:   Mon Apr 25 11:39:46 2016 +0900

    Merge pull request #120 from aqjune/zextsextand
    
    And 연산 관련 InstructionSimplify 꼬마최적화 6개 hint generation 코드 추가

commit a9775eb3adb9173e350fefd5807f010030d9fa0c
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Apr 25 11:37:05 2016 +0900

    Assert ValidationUnit::Exists at the beginning of each hint generation function

commit c29a0a058b36f9a1b5a15784b04c890890fd2807
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Mon Apr 25 00:22:21 2016 +0900

    Implement and_mone, and_undef, and_or, and_not, and_same, and_zero hint-gen code in InstructionSimplify

commit 68afd3a0ee3a8f70eb02b293bdd7d70b948701fc
Merge: f4a8c99 6925624
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Sun Apr 24 00:59:35 2016 +0900

    Resolve linking errors

commit f4a8c9943d9503c621d5c784f3e79d1ac7dd73d7
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri Apr 22 20:08:02 2016 +0900

    Commit all modified files for load-load prior to merging upstream

commit 6925624fafa16ed282862c9c5bf68f97ecd408ac
Merge: ff49a8a 3dd859c
Author: Jeehoon Kang <jeehoon.kang@sf.snu.ac.kr>
Date:   Fri Apr 22 19:00:58 2016 +0900

    Merge pull request #117 from aqjune/addopts
    
    InstCombine 최적화 4개 옮김

commit 3dd859cc7cc68cfd49e739d8c4fa785333523181
Merge: c6a33ca ff49a8a
Author: Juneyoung Lee <aqjune@gmail.com>
Date:   Fri Apr 22 18:57:21 2016 +0900

    Merge with master

commit ff49a8acf2c79b81d4ddb141021d20523ad35514
Merge: 61eed4d 14e29ea
Author: kim-yoonseung <yoonseung.kim@sf.snu.ac.kr>
Date:   Fri Apr 22 17:08:50 2016 +0900

    Merge pull request #119 from markshin/refactoring_foldPHIbin
    
    foldPHIbin const case 수정

commit 14e29ea7e11bfa734bc79b30919290833684f56b
Author: markshin <shin.dongyeon@gmail.com>
Date:   Fri Apr 22 17:02:35 2016 +0900

    change disassemble intrude

commit 61eed4d8e41755ba974f6263c2fa58087081da4e
Merge: 3229a67 a69dabc
Author: Jeehoon Kang <jeehoon.kang@sf.snu.ac.kr>
Date:   Fri Apr 22 16:47:57 2016 +0900

    Merge pull request #118 from markshin/refactoring_foldPHIbin
    
    foldPHIbin const case
