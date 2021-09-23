# **COP-5615 PROJECT 1**

Group Members:- 

    Saksham Bathla  UFID: 82967553

    Arnav Gaur      UFID: 83249716


## **Bitcoin Miner**

### **How to Run:**

  Single Machine:-
    
      dotnet build
      dotnet run 4    (4 is the number of preceding zeroes / difficulty as required)
  
  Distributed:- 

    Server : 
      dotnet fsi remote/server.fsx 5 (update the server hosting ip in line 34 in akka configuration)
    
    Client:
      dotnet fsi remote/client.fsx 10.20.0.250 (the ip is the server ip)


### **Implementation**
  We have used the concept of cryptographic nonce for generating unique strings and distributing the task load among the actors equally.
  Essentially each actor is provided with a prefix string ("bathlasaksham") and the min and max nonce to attach the nonce to the existing string and check 
  the hash for preceding zeroes
  
  #### Distributed Implementation:-
 
  ![diagram_1](https://user-images.githubusercontent.com/24275651/134545918-1dd970e4-bd70-4fa6-b69d-3a2bf3dd4c16.jpg)
    
  Client, upon startup, creates a list of Worker actors and sends a FirstContact message to the server with its identifier and reference to the list of workers. 
  Server, in return, (whether or not it is working on a local task or not) sends a mining task to each of the remote workers
  Remote worker, during working on the tasks, sends any successfully mined hash and string to the server as and when it finds one and continues working. 

### **Assignment Questions**

  1.) Size of the work unit that you determined: The optimal workunit was anything between 1000000 and 100000000. Below 10000000, there was not enough tasks at each worker 
      to utilize concurrency, above 10000000, workers took too long and throttled the CPU
  
  2.) The result of running your program for input 4: (on a windows Quad core i5 machine)

        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f32231772 0000cdc2662359119fc89871c714aeb9df6b782058b45a3f0b0f6bf817a55989
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3223038959 00000c50566f2a673b96d235d359305944c628f3fbb111a2d64d9bc04251eafe
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221550710 0000c2e76c1d94d1c13929bd9ff32e363067212821fc7b21fca7fe9092c38e7c
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221550727 0000ba6ef7d9188146260f4095e265a12dacaf47fbad2973b0b82a6b30208321
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3222051772 0000b75f6c42ad64d83281ea46d4d2171479ea035aa06a19d7c2f3e1ea8c450f
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3223061217 00008b5d70997136d7dfb2db42aa8d685f6c540adbe1c0d4360ee1b234704d64
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322566990 0000b59302a5a17ab124db1640f97b6f5b86e81c4161446691fc44e2fbacec61
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3223566870 0000ca1fabf9a10e49a49ce4691d444118f9dbd8c65cb5e0c223e9e6685bf557
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3222073389 0000afa69c8f2b71060096d5579484e865f52ee82a1fef4e49fe92d566e4dcea
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3223580233 00000c25148af3d3d0ae34ad9036745d273afe3960458a37f5f3866c8e14606f
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3223600230 0000eb93ec314ac4ff446e34324cde28500d7b26e73bbfdae1e07778f74a74df
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322625441 00008f218ffaa8861ff7bd4f80a42dc27abba30d21b2aee68bb0052d4248a11b
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3223630960 0000540c115d2fdc44daefbe64930d2c2ea21db12b910cd73f75c93dcae34930
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322662354 0000593c586c9942f5cf152e1735f6afc434626fff9e78b6a82db012184c375d
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322162472 00009f0d839e9926cc4618db5afe67cdef7760a79d0e5176611ab868e3a2e0c8
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322169341 0000c9541e55db5b6b6608f53783bc2bca6d9b3d1a5fcc972de091a510c66a22
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221677728 000027001fbf53a330b1a27663e2f84bb4731990f558373efb3db1ce6eafaf39
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221186280 000085bb99ce7591854ace6530b30421dcd05283f62c884acbc2ce395fe1ac52
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221197818 0000ccb7a35f9bf441e48026a48bf948aba1cf93cb1a7a74922a4ea6f7ec92a4
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3223695184 00001c00fb6940d024ddaed531b52512d471f7f515007a8fcf03b8a8faaeb504
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322203078 00002130e2007d254ed13f76df340072041eb20c1240595eec6cfeaa67557377
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322707607 0000b60a28d4e6edaeaef55fd5c06e1dca6cf76acf366ea9b7ef4f14c5b80a27
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3222715026 000071f422fadb58582fada0dcd75d703649177f614c8843b350027d3607e7bf
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322716447 0000503bf0b88934067c0782466c10c63379079610a8c43e077a279959880c17
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221718304 0000186ff0917c5141af7c85cb4af66ec38bb0b57841e34df3c9877bb31393af
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221722044 0000b6aa1b2197133f996d6de5ab5251b5840c02d4bf8e7fd2a71feec05ac9f1
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3223226852 0000e3a36dda0427f50f3561840edd34a0182ece337b640453276d85d448a50d
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221249659 0000f71511b202c37e60428213ba6a2d10f4c0f6250d2454cbc208a10bc3f51a
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3222758339 00003f6f7234e966cf3c6a27b3366231a45530576b9168cd4131a9cffb563e9e
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221269425 0000323c249661efc1c8ac8e1e44333d7d6bc26ad22317791f90258eb1621ae0
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322767106 0000be79bf0d9bc8b0395d090643f7cde14754e23657b31e5c770c1215ec26ed
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322269787 0000be379c2e75393230a3e8b4767298857bbcd0e2b9c5423ac75630a3b66f7f
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221279907 0000dddee96ec9f9fc3259e4aa53788d41fe02768080a59e616f5c9e7fef850c
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322291347 0000af1ef366645063b7a14115a46b5a326b3a77de46d105f58ccb291e97c00e
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221788019 00003094eaa8d76e60bf1ebb87aaeb6b79a6012bbdf90733a8134a923e889253
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322295139 00003e55c1323c47d9acc887a669ad1e8e47429ca1887a496f936e27589fbcca
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221308975 0000fc4f49d50a2f47ad6917929b8ab669d8bb221ce9c83eda94760efd268f26
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3223807216 000041703a33bf00227bd615833506dc0b23ba0817461e601c7dc047f602f8f3
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3222819677 0000dc489c817a5ca4c9bd7acac6fc3efe169147e097ce4a8c9c2e724d150636
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322835491 00005cacd32208efc11b660e5ed6b385c41b4c000024389162e71df941814462
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221854444 0000fbd28abf64b59a67015fb2d095e8d0cef50dff6fc38ac927ad10e0fc5dae
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221857635 0000c62620f5c35a06ca04f58ecd21d1d9c4f2804182801754092ed34b069a04
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3223359446 0000d614ea8d232c3f24e1e96389fa264aa8a56aea5c604cd70683cf54a74cf4
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322374665 0000072f5faad89c9e2dbe5e14568ea5208b6e44f2bfc3ea99ba8fc244a15e61
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221876811 00009455c83303980be33cd2a972b36e2f265f7551cd846e83164bc65e120fb9
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3223891706 00004e9e3ca3982fe0965a86d064295f946fce12ffa77ddf633ea8ba3739b49a
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3221420102 0000604cc8102cd0e6990f841e5e1c242982a5fc88ae2d7af2c2ce1bc8f8c5f7
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3222430670 0000f586361ed71ac78d2c3b8f30ec6a703114c9375e7c03db667be2cb574ab2
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322420995 00003f237b2c35cf7a821c3886907a8a2682db0a0d3ed6518b087d2ec04dfb3b
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3223913869 000088c43a66db6c335724dddf50e511b1da6903c4ac55a10828c27de6b42901
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3222931277 00009e14879148c06b37c4be1386a5c7956fc8d37888558b5043f322a41d2ea0
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322936752 0000cc48a1f97a2d46a1eb704f3ea8f1bddf1ad7be41aa1235d57cc221ccccce
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3222463457 00005b1e3694b24fd2ed666b2632118a1c993d1be3d20f2c66e7fb7ea1732eb1
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322483797 0000f90e3c18c97d321824baecb0c58673cb6cb9b3822228f5ce215e438631e8
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322498731 000015462147f55a8b84d1a7e4044acb5af27126c17b4b1c4afcbb781ac46a5c
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f3223484306 00006865133f5d9995a1f11d2afe11f65c741493d5d0624b9b3ed5647b46c3c5
        bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322991528 0000a5074865df7b3622240089f2e1c6db411c32fdaafb1af865d67121ff19aa
        
        
   3.) Running Time
 
        CPU time = 25687ms
        Absolute time = 5814ms
        ratio: 4.418129
  
   4.) Largest no of zeros found was 7. It took roughly 4 minutes to compute

          bathlasaksham44df292016f45d851f552274002b231ea4a8748a890fcf850d102f0e45c1f322120669116 00000003ab6c88c98678dd748e48f9a1acbeef29606fcff043df215047a04b96
          CPU time = 1173656ms
          Absolute time = 256301ms
          ratio: 4.579192

   5.) Largest no of machines we were able to run this code on: 3 (one i5 quad core, one apple m1 and one ryzen 5 hexa core) 
       We had access to only three machines but it can scale over 3 very easily