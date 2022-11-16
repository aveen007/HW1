(* 1******* *)

datatype card_type=king|queen|jack|ace|num of int;;
datatype card_group=hearts|spades|diamonds|clubs;;
datatype card= card_pair of card_group * card_type;;
datatype color =red|black
(* 2 ********* *)

fun card_color((hearts,_))=red
    |card_color((diamonds,_))=red
    |card_color(card)=black
val c=(clubs,num 7)
val colored=card_color(c)
(* 3 ********* *)


fun card_value((_,num i))=i
    |card_value((_,ace))=11
    |card_value(card)=10

val valued=card_value(c)
(* 4 ********* *)

fun remove_card(c1::tcd,c2, acc)= if c1=c2 then acc@tcd else remove_card(tcd,c2,c1::acc)
    |remove_card([],c1,acc)=acc


val cards=([(spades,king),(spades,num 8),(clubs,num 7)])
val cardss=remove_card(cards,c,[])

(* 5 ********* *)
fun is_same_color([])=true
    |is_same_color([c1])=true
    |is_same_color(c1::c2::cs)=
     if card_color(c1)=card_color(c2) then  true andalso is_same_color(cs)
     else   false
val is_colored=is_same_color(cards)


(* 6 ********* *)
fun sum_cards([])=0
    |sum_cards(c1::tcd)=card_value(c1)+sum_cards(tcd)

  val summed=  sum_cards(cards)

