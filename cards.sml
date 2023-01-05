(* Aveen Hussein *)
(* 1******* *)

datatype card_type=king|queen|jack|ace|num of int;;
datatype card_group=hearts|spades|diamonds|clubs;;
datatype card= card_pair of card_group * card_type;;
datatype color =red|black
(* 2 ********* *)

fun card_color(card_pair(hearts,_))=red
    |card_color(card_pair(diamonds,_))=red
    |card_color(card)=black
val c=card_pair(clubs,queen)
val colored=card_color(c)
(* 3 ********* *)


fun card_value(card_pair(_,num i))=i
    |card_value(card_pair(_,ace))=11
    |card_value(card)=10

val valued=card_value(c)
(* 4 ********* *)

exception IllegalMove 
fun remove_card(c1::tcd,c2, acc, e)= if c1=c2 then acc@tcd else remove_card(tcd,c2,c1::acc, e)
    |remove_card([],c1,[], e)= raise e
    |remove_card([],c1,acc, e)=raise e


val cards=([card_pair(spades,king),card_pair(spades,num 8),c])
val cardss=remove_card(cards,c,[],IllegalMove )


(* 5 *********)
fun is_all_same_color([])=true
    |is_all_same_color([c1])=true
    |is_all_same_color(c1::c2::cs)=
     if card_color(c1)=card_color(c2) then  true andalso is_all_same_color(cs)
     else   false
val is_colored=is_all_same_color(cards)


(* 6 ********* *)
fun sum_cards([])=0
    |sum_cards(c1::tcd)=card_value(c1)+sum_cards(tcd)

  val summed=  sum_cards(cards)

(* 7 ********* *)

fun score(goal,cards)=  let val prim_score= 
                        if sum_cards(cards)>goal then 3*(sum_cards(cards)-goal) 
                        else goal-(sum_cards(cards))
                        in  if is_all_same_color(cards) then prim_score div 2
                        
                        else prim_score
                        end
val scored=score(3,cards)

(* 8 ********* *)
 datatype move=discard of card|draw
(* 9 ********* *)
fun officiate(c1::card_list, goal,draw::moves,cards )=
    if sum_cards( c1::cards)>goal then score(goal,c1::cards) else
        officiate(card_list, goal,moves, c1::cards)
    |officiate([], goal,draw::moves,cards )=score(goal,cards)
    |officiate(card_list, goal,discard c2::moves,cards)=officiate(card_list,goal,moves,remove_card(cards,c,[],IllegalMove))
    |officiate(card_list, goal,[],cards)=score(goal,cards)


val gamed =
  [  (  officiate([card_pair(clubs,jack),card_pair(spades,num(8))],42, [draw,discard c],[] ) handle IllegalMove=>9999)  = 9999
     , officiate([card_pair(clubs,ace),card_pair(spades,ace),card_pair(clubs,ace),card_pair(spades,ace),c],42, [draw,draw,draw,draw,draw],[])=3
     , officiate([card_pair(clubs,ace),card_pair(spades,ace),card_pair(clubs,ace),card_pair(spades,ace)],30, [draw,draw,draw,draw,draw],[])=4
     , officiate([card_pair(clubs,ace),card_pair(spades,ace),card_pair(clubs,ace),card_pair(spades,ace)],22, [draw,draw,draw,draw,draw],[])=16
     , officiate([card_pair(clubs,ace),card_pair(spades,ace),card_pair(clubs,ace),card_pair(spades,ace)],100, [draw,draw,draw,draw,draw],[])=28
     , officiate([card_pair(clubs,ace),card_pair(spades,ace),card_pair(clubs,ace),card_pair(spades,ace)],44, [draw,draw,draw,draw,draw],[])=0
     , officiate([card_pair(diamonds,ace),card_pair(spades,ace),card_pair(clubs,ace),card_pair(spades,ace)],30, [draw,draw,draw,draw,draw],[])=4
     , officiate([card_pair(clubs,ace),card_pair(hearts,ace),card_pair(clubs,ace),card_pair(spades,ace)],22, [draw,draw,draw,draw,draw],[])=33
     , officiate([card_pair(clubs,ace),card_pair(spades,ace),card_pair(diamonds,ace),card_pair(spades,ace)],100, [draw,draw,draw,draw,draw],[])=56
     , officiate([card_pair(clubs,ace),card_pair(spades,ace),card_pair(clubs,ace),card_pair(hearts,ace)],44, [draw,draw,draw,draw,draw],[])=0
     , officiate([card_pair(clubs,ace),card_pair(diamonds,ace),card_pair(clubs,ace),card_pair(hearts,ace)],30 ,[draw,draw],[])=8
     , officiate([card_pair(clubs,ace),card_pair(diamonds,ace),card_pair(clubs,ace),card_pair(hearts,ace)], 22,[draw,draw],[])=0
     , officiate([card_pair(clubs,ace),card_pair(diamonds,ace),card_pair(clubs,ace),card_pair(hearts,ace)],11, [draw,draw],[])=33
     , officiate([card_pair(clubs,queen),card_pair(diamonds,ace),card_pair(clubs,ace),card_pair(hearts,ace)], 11,[draw,discard c,draw,draw],[])=33
     , officiate([card_pair(clubs,queen),card_pair(diamonds,ace),card_pair(clubs,ace),card_pair(hearts,ace)],22, [draw,discard c,draw,draw],[])=0
     , officiate([card_pair(clubs,queen),card_pair(diamonds,ace),card_pair(clubs,ace),card_pair(hearts,ace)],30, [draw,discard c,draw,draw],[])=8
     , officiate([card_pair(clubs,queen),card_pair(diamonds,ace),card_pair(hearts,ace),card_pair(diamonds,ace)],11, [draw,discard c,draw,draw],[])=16
     , officiate([card_pair(clubs,queen),card_pair(diamonds,ace),card_pair(hearts,ace),card_pair(diamonds,ace)],22, [draw,discard c,draw,draw],[])=0
     , officiate([card_pair(clubs,queen),card_pair(diamonds,ace),card_pair(hearts,ace),card_pair(diamonds,ace)],30, [draw,discard c,draw,draw],[])=4
     , officiate([card_pair(clubs,queen),card_pair(diamonds,ace),card_pair(hearts,ace),card_pair(diamonds,ace)], 11,[draw,draw,discard c,draw],[])=30
     , officiate([card_pair(clubs,queen),card_pair(diamonds,ace),card_pair(hearts,ace),card_pair(diamonds,ace)],22, [draw,draw,discard c,draw],[])=0
     , officiate([card_pair(clubs,queen),card_pair(diamonds,ace),card_pair(hearts,ace),card_pair(diamonds,ace)],30, [draw,draw,discard c,draw],[])=4
  ]
    val v=officiate([card_pair(diamonds,ace),card_pair(spades,ace),card_pair(clubs,ace),card_pair(spades,ace)],30, [draw,draw,draw,draw,draw],[])


 

