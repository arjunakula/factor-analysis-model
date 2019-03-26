Enigma <- function(input, rotor.start = "AAA", ring = "AAA",
                   rotor.config = c(1,2,3), reflector = "B"){
  
  # Emulator of the German M3 Enigma Machine.
  
  # In addition to the message to be encrypted/decrypted, 'input', the user of
  # the machine needs to set (or leave as default) the following four:
  #  (1) the 'rotor.start' position, which is three letter combination, for
  #    example "ERT", "PWQ", or "TMM",
  #  (2) the 'ring' setting, which is also a three letter combination,
  #  (3) the 'rotor.config', three of the eight Enigma machine rotors in order
  #    from left to right, supplied as a numeric vector, for example c(1,2,3),
  #    c(6,4,3), or c(8,1,2),
  #  (4) the 'reflector', of which there existed two variants, called "B" and
  #    "C", default is "B".
  
  # Note that the Enigma machine was delivered with only one rotor of each, so
  # in practice rotor orders such as c(1,1,1) or c(4,2,2) could not be used.
  # However, this emulator does not have that restriction.
  
  # Note also that no letter can be encrypted into itself, which allowed for
  # quick so-called 'crib dragging', which sped up brute force attacks greatly.
  # A crib is a snippet of text presumed to be included in the decrypted
  # message, for example "WEATHERREPORT". Another weakness of the Enigma
  # machine is the ordered one-to-one correspondence between letters in the
  # encrypted and decrypted messages; if one letter is changed in the decrypted
  # message, it will only affect the one letter in the same position of the
  # encrypted message.
  
  m <- function(x){ ((x-1) %% 26) +1 }
  
  alph <- unlist(strsplit(rawToChar(as.raw(65:90)),split = ""))
  
  ring.pos <- which(alph == unlist(strsplit(toupper(ring), split = ""))[1])-1
  ring.pos[2] <- which(alph == unlist(strsplit(toupper(ring), split = ""))[2])-1
  ring.pos[3] <- which(alph == unlist(strsplit(toupper(ring), split = ""))[3])-1
  
  if(toupper(reflector) == "B"){refl.num <- 11}
  if(toupper(reflector) == "C"){refl.num <- 12}
  
  rotor.data <- list(I = rbind(alph,unlist(strsplit("EKMFLGDQVZNTOWYHXUSPAIBRCJ",split = ""))),
                     II = rbind(alph,unlist(strsplit("AJDKSIRUXBLHWTMCQGZNPYFVOE",split = ""))),
                     III = rbind(alph,unlist(strsplit("BDFHJLCPRTXVZNYEIWGAKMUSQO",split = ""))),
                     IV = rbind(alph,unlist(strsplit("ESOVPZJAYQUIRHXLNFTGKDCMWB",split = ""))),
                     V = rbind(alph,unlist(strsplit("VZBRGITYUPSDNHLXAWMJQOFECK",split = ""))),
                     VI = rbind(alph,unlist(strsplit("JPGVOUMFYQBENHZRDKASXLICTW",split = ""))),
                     VII = rbind(alph,unlist(strsplit("NZJHGRCXMYSWBOUFAIVLPEKQDT",split = ""))),
                     VIII = rbind(alph,unlist(strsplit("FKQHTLXOCBJSPDZRAMEWNIUYGV",split = ""))),
                     Beta = rbind(alph,unlist(strsplit("LEYJVCNIXWPBQMDRTAKZGFUHOS",split = ""))),
                     Gamma = rbind(alph,unlist(strsplit("FSOKANUERHMBTIYCWLQPZXVGJD",split = ""))),
                     reflectorB = rbind(alph,unlist(strsplit("YRUHQSLDPXNGOKMIEBFZCWVJAT",split = ""))),
                     reflectorC = rbind(alph,unlist(strsplit("FVPJIAOYEDRZXWGCTKUQSBNMHL",split = ""))))
  
  turnover.data <- list(I=16, II=4, III=21, IV=9, V=25, VI=c(12,25), VII=c(12,25), VIII=c(12,25))
  
  rotors <- list(rotor1 = rotor.data[[rotor.config[1]]],
                 rotor2 = rotor.data[[rotor.config[2]]],
                 rotor3 = rotor.data[[rotor.config[3]]],
                 reflector = rotor.data[[refl.num]])
  
  turns <- c(turnover.data[rotor.config[1]],turnover.data[rotor.config[2]],turnover.data[rotor.config[3]])
  
  rotor.pos <- (which(alph == unlist(strsplit(toupper(rotor.start), split = ""))[1])-1-ring.pos[1])%% 26
  rotor.pos[2] <- (which(alph == unlist(strsplit(toupper(rotor.start), split = ""))[2])-1-ring.pos[2])%% 26
  rotor.pos[3] <- (which(alph == unlist(strsplit(toupper(rotor.start), split = ""))[3])-1-ring.pos[3])%% 26
  
  in_tmp <- toupper(input)
  in_tmp <- gsub(" ","",in_tmp)
  in_tmp <- gsub("[[:punct:]]", "", in_tmp)
  in_tmp <- unlist(strsplit(in_tmp, split = ""))
  
  out <- character(0)
  while(length(in_tmp) > 0){
    
    if(any((rotor.pos[2]+ring.pos[2])%%26 == turns[[2]])){
      rotor.pos[1] <- m(rotor.pos[1]+1)
      rotor.pos[2] <- m(rotor.pos[2]+1)
    }
    if(any((rotor.pos[3]+ring.pos[3])%%26 == turns[[3]]) & all((rotor.pos[2]+ring.pos[2])%%26 != turns[[2]])){
      rotor.pos[2] <- m(rotor.pos[2]+1)
    }
    rotor.pos[3] <- m(rotor.pos[3]+1)
    
    a <- in_tmp[1]
    a <- rotors$rotor3[2,m(which(alph == a)+rotor.pos[3])]
    a <- rotors$rotor2[2,m(which(rotors$rotor3[1,] == a)-rotor.pos[3]+rotor.pos[2])]
    a <- rotors$rotor1[2,m(which(rotors$rotor2[1,] == a)-rotor.pos[2]+rotor.pos[1])]
    a <- rotors$reflector[2,m(which(rotors$rotor1[1,] == a)-rotor.pos[1])]
    a <- rotors$rotor1[1,m(which(rotors$reflector[1,] == a)+rotor.pos[1])]
    a <- rotors$rotor2[1,m(which(rotors$rotor1[2,] == a)-rotor.pos[1]+rotor.pos[2])]
    a <- rotors$rotor3[1,m(which(rotors$rotor2[2,] == a)-rotor.pos[2]+rotor.pos[3])]
    a <- alph[m(which(rotors$rotor3[2,] == a)-rotor.pos[3])]
    out <- c(out,a)
    
    in_tmp <- in_tmp[-1]
  }
  return(paste(out,collapse = ""))
}

next_key <- function(x){
  
  # Utility function useful for the Enigma emulator. Function accepts a
  # three letter character string or vector, e.g. 'AAA', and returns the
  # next three letter character string in lexicographical order. For
  # example, 'AAA' become 'AAB', and 'DZZ' becomes 'EAA'.
  # Note that this function does not replicate the rotor steppings of
  # the Enigma machine, which depend on the rotor configuration used.
  
  tmp <- unlist(strsplit(toupper(x),split = ""))
  alph <- unlist(strsplit(rawToChar(as.raw(65:90)),split = ""))
  tmp_v <- c(which(alph == tmp[1]),which(alph == tmp[2]),which(alph == tmp[3]))
  
  tmp_v[3] <- tmp_v[3]+1
  if(tmp_v[3] == 27){ tmp_v[2] <- tmp_v[2]+1; tmp_v[3] <- 1}
  if(tmp_v[2] == 27){ tmp_v[1] <- tmp_v[1]+1; tmp_v[2] <- 1}
  if(tmp_v[1] == 27){ tmp_v <- c(1,1,1)}
  
  return(paste(alph[tmp_v],collapse = ""))
}

# for(k1 in 1:26){
#   print("k1")
#   print(k1)
#   for(k2 in 1:26){
#     
#     print("k2")
#     print(k2)
#     
#     for(k3 in 1:26){
# 
#     rotor_start = paste(LETTERS[k1], LETTERS[k2], LETTERS[k3], sep='');
#     
#     for (i1 in 1:26){
#       for(j1 in 1:26){
#         rotor_setting = paste(LETTERS[i1], LETTERS[j1], 'X', sep='')
#         
#         if(Enigma("ENDOFTRANSMISSION",rotor_start,rotor_setting) == "YZPLRHCNOUENRPFIG"){
#           print(rotor_start);
#           print(rotor_setting);
#           stop("Did it!!!")
#         }
#       }
#     }
#   }
# }
# }

# code_str = "MAKTFBFITKJWRPWRMCTNBZJPPOPNIJKURHDYIOTEYHFOTYWEZJYAVZZVQHLLFTYBWZQGQNIGQVCLAHYGWODPWFRHXYNAYKULAFZRTHBEEBYGIGSMFNCDDBTCIJJNPDNFZJZHMUNFHHJHHRPBHDTSLMWGWXTABPNUDFVNCRQCXWHQWVJNENKXUNVGVVLHMGCFGTMZYEVFDFTAYQJDZKHSGAAJSXTYPEHGJTYLOVDIQRLHTZIXYZPLRHCNOUENRPFIG";
# 
# for (i1 in 1:26){
#   print(i1);
#   for(j1 in 1:26){
#     for(k1 in 1:26){
#     rotor_start = paste(LETTERS[i1], LETTERS[j1], LETTERS[k1], sep='')
# 
#     if(substr(Enigma(code_str,rotor_start),241,257) == "ENDOFTRANSMISSION"){
#       print(rotor_start);
#       stop("Found the code!!!")
#     }
#     }
#   }
# }

# code_str1 = "KRK";
# code_str2 = "KRKWPXMBNFZVFGGJXJIXDODQCWLLLXZPKSRPOJQQDNEPRXXHCSCMGIOBKYASWDCYRLYBGNAZHSCGFSEQYTWVXMQWYACJLSNRFAAMDCADLUSYOUTPWE";
# 
# for (z1 in 1:26){
#   for(z2 in 1:26){
#     print(z1);
#     print(z2);
#   
#     for(z3 in 1:26){
#       
#       ring = paste(LETTERS[z1], LETTERS[z1], LETTERS[z3], sep='')
#       
#       for (i1 in 1:26){
#         for(j1 in 1:26){
#           for(k1 in 1:26){
#             rotor_start = paste(LETTERS[i1], LETTERS[j1], LETTERS[k1], sep='')
#             
#             if(Enigma(code_str1,rotor_start,ring) == "ANX"){
#               #print(rotor_start);
#               #print(ring);
#               #print("Found the begin code!!!");
#               
#               if(substr(Enigma(code_str2,rotor_start,ring),105,114) == "YOURSTRULY"){
#                 print(rotor_start);
#                 print(ring);
#                 stop("Found the end code!!!")
#               }
#             }
#           }
#         }
#       }
#     }
#   }
# }  


#load("V:/arjun_google_drive_docs/CourseWork/UCLA_Winter_2017/202b/HW1/EnigmaFreq.RData")
#code_str1 = "KRK";
# code_str2 = "DMSBSWYGFPUMKFXFLUAYSISJSOCSRAZEBRTCMPEBBKNIEUIWECUAIIUDTUTGEHCKRTDTWAEPEZUFODTPTKHPHFHLTDIKLNVFLJEYYRLYXCMFOFLWZMEFECOUHYPVNGXFHNNQHFRWJTDHZCXQPKOSGEBKEHCPTYYIIXJJDUFJVAECTZCATWMXCQGZRVQNUZKJWECJOZTUJSYATCCEJTUOBPKTHKCUKQQFKUYSUMAFFAAJOFXJYTZITOBLKYOZOMIFIBHZTCAMNPDEYMAVYDUABBDFEYXBGHUDBNAUCXQADINCMGSSLJKDEVXIKZQKMFMJEIQBJPYJQEHNGFPYNAUGYYFQNXCJGAFHQIYHEWAPFSIHIJUXXQINCRRRVABCRSXAOXNKKPYPZUDGHJFPDWGALQAEYZFUUJEKLIJQEEEHNZVISUCDWOQOZTKAFBZKOBBYMEKWKSVVYQRPRLSQGHUVTMCCLCTUISDTYRWHUZFRUOPMIIWTGZIHABKZCRGQSKFUYGKVFDRTSROGEICPTCDHSIVBDSIBGJUGJPFUMKPDEZINLMOTRMXUGNTDQJFJLHCQQYDNCWMRPKEAQZQQLFTKFKDLUXHGMFRZYGRFIVWGRRUNRPPNUGDUTBCDYLBKAUKRDOAPNMEJRBZNQVSCCMZABLSBAIHJZOANIFBCNCSQRAPAPKUQPUOSMJOERKTKOGBYJUPZITKVLCLGRUTOMUCIKTVXCNQKLRBGDYJCPTBOUQLTWWCMCLNHKROFXQGJARRWJRKBZOHXZCRQROZDTBJLZWOCOMENBYAORXTFZEJYFHSGWCQZBYIGEPESPSZSWIZRKOCEUQSREAQEEJVGDIVKYEOSAVTSWYLZCVHRASERSWIIFYUTUJGZQGPUAXGKJJRDLVWIKWQPGRVDDD";
# 
# freq_orig = as.vector(letterfreq)
# rotor_score = c();
# rotor_names = c();
# 
# cnt = 0;
# ring = "AAA";
# for (i1 in 1:26){
#   print(i1)
#   for(j1 in 1:26){
#     
#     for(k1 in 1:26){
#       
#       rotor_start = paste(LETTERS[i1], LETTERS[j1], LETTERS[k1], sep='');
#       
#       decipher = Enigma(code_str2,rotor_start,ring);
#       
#       cr_val = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0); 
#       names(cr_val) = c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z");
#       for (i in 1:nchar(decipher)){
#         cr_val[substr(decipher,i,i)] = cr_val[substr(decipher,i,i)]+1  
#       }
#       
#       cr_vec_val = as.vector(cr_val);
#       
#       distance = sqrt(sum((cr_vec_val-freq_orig)^2));
#       
#       rotor_score = append(rotor_score,distance);
#       rotor_names = append(rotor_names,rotor_start);
#       
#     }
#   }
# }
# 
# names(rotor_score) = rotor_names;
# rotor_score_final = rotor_score[order(unlist(rotor_score))]
# 
# fileConn<-file("output_final.txt")
# write(rotor_score_final, fileConn)
# close(fileConn)
# 
# fileConn<-file("output_final2.txt")
# write(names(rotor_score_final), fileConn)
# close(fileConn)


load("V:/arjun_google_drive_docs/CourseWork/UCLA_Winter_2017/202b/HW1/EnigmaFreq.RData")
code_str1 = "DMSBSWYGFPU";
code_str2 = "DMSBSWYGFPUMKFXFLUAYSISJSOCSRAZEBRTCMPEBBKNIEUIWECUAIIUDTUTGEHCKRTDTWAEPEZUFODTPTKHPHFHLTDIKLNVFLJEYYRLYXCMFOFLWZMEFECOUHYPVNGXFHNNQHFRWJTDHZCXQPKOSGEBKEHCPTYYIIXJJDUFJVAECTZCATWMXCQGZRVQNUZKJWECJOZTUJSYATCCEJTUOBPKTHKCUKQQFKUYSUMAFFAAJOFXJYTZITOBLKYOZOMIFIBHZTCAMNPDEYMAVYDUABBDFEYXBGHUDBNAUCXQADINCMGSSLJKDEVXIKZQKMFMJEIQBJPYJQEHNGFPYNAUGYYFQNXCJGAFHQIYHEWAPFSIHIJUXXQINCRRRVABCRSXAOXNKKPYPZUDGHJFPDWGALQAEYZFUUJEKLIJQEEEHNZVISUCDWOQOZTKAFBZKOBBYMEKWKSVVYQRPRLSQGHUVTMCCLCTUISDTYRWHUZFRUOPMIIWTGZIHABKZCRGQSKFUYGKVFDRTSROGEICPTCDHSIVBDSIBGJUGJPFUMKPDEZINLMOTRMXUGNTDQJFJLHCQQYDNCWMRPKEAQZQQLFTKFKDLUXHGMFRZYGRFIVWGRRUNRPPNUGDUTBCDYLBKAUKRDOAPNMEJRBZNQVSCCMZABLSBAIHJZOANIFBCNCSQRAPAPKUQPUOSMJOERKTKOGBYJUPZITKVLCLGRUTOMUCIKTVXCNQKLRBGDYJCPTBOUQLTWWCMCLNHKROFXQGJARRWJRKBZOHXZCRQROZDTBJLZWOCOMENBYAORXTFZEJYFHSGWCQZBYIGEPESPSZSWIZRKOCEUQSREAQEEJVGDIVKYEOSAVTSWYLZCVHRASERSWIIFYUTUJGZQGPUAXGKJJRDLVWIKWQPGRVDDD";

freq_orig = as.vector(letterfreq)
rotor_score = c();
rotor_names = c();

cnt = 0;

textfile = paste(readLines("C:/Users/lbsswu/Music/names.txt"), collapse="\n")
rotors = strsplit(textfile,"\n")[[1]]

#for ( i1 in 1:10){

# print(i1);
#rotor_start = rotors[i1];
#rotor_start = "YGX";

for (z1 in 1:26){
  
  print(z1);
  for(z2 in 1:26){
    #print(z2);
    
    for(z3 in 1:26){
      #print(z3);    
      ring = paste(LETTERS[z1], LETTERS[z1], LETTERS[z3], sep='')
      
      for (i1 in 1:1){
        #print(i1)
        for(j1 in 9:9){
          
          for(k1 in 1:26){
            
            rotor_start = paste(LETTERS[i1], LETTERS[j1], LETTERS[k1], sep='');
            
            
            
            
            
            decipher_short = Enigma(code_str1,rotor_start,ring);
            
            if(decipher_short == "THEMIGHTIESTMEN"){
              
              decipher = Enigma(code_str2,rotor_start,ring);
              
              print(decipher)
              
              if(grepl("STRANGE",decipher) & grepl("DIVINE",decipher) & grepl("THEMIGHTYONESOFTHEWORLDLEARNEDTOHAVE",decipher) & grepl("WISH",decipher) & grepl("REVERENTLYBEFORETHESAINT",decipher) & grepl("THEENIGMAOF",decipher)){
                
                print(substr(decipher,890,909));
                print(ring);
                print(rotor_start)
                stop();
                # cr_val = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
                # names(cr_val) = c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z");
                # for (i in 1:nchar(decipher)){
                #   cr_val[substr(decipher,i,i)] = cr_val[substr(decipher,i,i)]+1
                # }
                # 
                # cr_vec_val = as.vector(cr_val);
                # 
                # distance = sqrt(sum((cr_vec_val-freq_orig)^2));
                # 
                # rotor_score = append(rotor_score,distance);
                # rotor_names = append(rotor_names,rotor_start);
              }
            }
            
          }
        }
      }
    }
  }
}


# names(rotor_score) = rotor_names;
# rotor_score_final = rotor_score[order(unlist(rotor_score))]
# 
# fileConn<-file("output_final.txt")
# write(rotor_score_final, fileConn)
# close(fileConn)
# 
# fileConn<-file("output_final2.txt")
# write(names(rotor_score_final), fileConn)
# close(fileConn)
# 
# if(substr(Enigma(code_str2,rotor_start,ring),105,114) == "YOURSTRULY"){
#   print(rotor_start);
#   print(ring);
#   stop("Found the end code!!!")
# }

# finding cribs:
