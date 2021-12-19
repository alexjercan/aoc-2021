{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day19Test where

import Day19
import Util.Transform
import Test.Framework

sampleInput1 :: Input
sampleInput1 = [Scanner Nothing [Point3D 404 (-588) (-901),Point3D 528 (-643) 409,Point3D (-838) 591 734,Point3D 390 (-675) (-793),Point3D (-537) (-823) (-458),Point3D (-485) (-357) 347,Point3D (-345) (-311) 381,Point3D (-661) (-816) (-575),Point3D (-876) 649 763,Point3D (-618) (-824) (-621),Point3D 553 345 (-567),Point3D 474 580 667,Point3D (-447) (-329) 318,Point3D (-584) 868 (-557),Point3D 544 (-627) (-890),Point3D 564 392 (-477),Point3D 455 729 728,Point3D (-892) 524 684,Point3D (-689) 845 (-530),Point3D 423 (-701) 434,Point3D 7 (-33) (-71),Point3D 630 319 (-379),Point3D 443 580 662,Point3D (-789) 900 (-551),Point3D 459 (-707) 401],Scanner Nothing [Point3D 686 422 578,Point3D 605 423 415,Point3D 515 917 (-361),Point3D (-336) 658 858,Point3D 95 138 22,Point3D (-476) 619 847,Point3D (-340) (-569) (-846),Point3D 567 (-361) 727,Point3D (-460) 603 (-452),Point3D 669 (-402) 600,Point3D 729 430 532,Point3D (-500) (-761) 534,Point3D (-322) 571 750,Point3D (-466) (-666) (-811),Point3D (-429) (-592) 574,Point3D (-355) 545 (-477),Point3D 703 (-491) (-529),Point3D (-328) (-685) 520,Point3D 413 935 (-424),Point3D (-391) 539 (-444),Point3D 586 (-435) 557,Point3D (-364) (-763) (-893),Point3D 807 (-499) (-711),Point3D 755 (-354) (-619),Point3D 553 889 (-390)],Scanner Nothing [Point3D 649 640 665,Point3D 682 (-795) 504,Point3D (-784) 533 (-524),Point3D (-644) 584 (-595),Point3D (-588) (-843) 648,Point3D (-30) 6 44,Point3D (-674) 560 763,Point3D 500 723 (-460),Point3D 609 671 (-379),Point3D (-555) (-800) 653,Point3D (-675) (-892) (-343),Point3D 697 (-426) (-610),Point3D 578 704 681,Point3D 493 664 (-388),Point3D (-671) (-858) 530,Point3D (-667) 343 800,Point3D 571 (-461) (-707),Point3D (-138) (-166) 112,Point3D (-889) 563 (-600),Point3D 646 (-828) 498,Point3D 640 759 510,Point3D (-630) 509 768,Point3D (-681) (-892) (-333),Point3D 673 (-379) (-804),Point3D (-742) (-814) (-386),Point3D 577 (-820) 562],Scanner Nothing [Point3D (-589) 542 597,Point3D 605 (-692) 669,Point3D (-500) 565 (-823),Point3D (-660) 373 557,Point3D (-458) (-679) (-417),Point3D (-488) 449 543,Point3D (-626) 468 (-788),Point3D 338 (-750) (-386),Point3D 528 (-832) (-391),Point3D 562 (-778) 733,Point3D (-938) (-730) 414,Point3D 543 643 (-506),Point3D (-524) 371 (-870),Point3D 407 773 750,Point3D (-104) 29 83,Point3D 378 (-903) (-323),Point3D (-778) (-728) 485,Point3D 426 699 580,Point3D (-438) (-605) (-362),Point3D (-469) (-447) (-387),Point3D 509 732 623,Point3D 647 635 (-688),Point3D (-868) (-804) 481,Point3D 614 (-800) 639,Point3D 595 780 (-596)],Scanner Nothing [Point3D 727 592 562,Point3D (-293) (-554) 779,Point3D 441 611 (-461),Point3D (-714) 465 (-776),Point3D (-743) 427 (-804),Point3D (-660) (-479) (-426),Point3D 832 (-632) 460,Point3D 927 (-485) (-438),Point3D 408 393 (-506),Point3D 466 436 (-512),Point3D 110 16 151,Point3D (-258) (-428) 682,Point3D (-393) 719 612,Point3D (-211) (-452) 876,Point3D 808 (-476) (-593),Point3D (-575) 615 604,Point3D (-485) 667 467,Point3D (-680) 325 (-822),Point3D (-627) (-443) (-432),Point3D 872 (-547) (-609),Point3D 833 512 582,Point3D 807 604 487,Point3D 839 (-516) 451,Point3D 891 (-625) 532,Point3D (-652) (-548) (-490),Point3D 30 (-46) (-14)]]

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 79 $ solve1 sampleInput1

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 3621 $ solve2 sampleInput1
