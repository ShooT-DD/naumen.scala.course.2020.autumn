package homework_3

import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_prettyBooleanFormatter1 - {
            assert(Exercises.prettyBooleanFormatter1(true) == "правда")
            assert(Exercises.prettyBooleanFormatter1(false) == "ложь")
            assert(Exercises.prettyBooleanFormatter1(777) == "777")
            assert(Exercises.prettyBooleanFormatter1("Shoot") == "Shoot")
        }
        'test_prettyBooleanFormatter2 - {
            assert(Exercises.prettyBooleanFormatter2(true) == "правда")
            assert(Exercises.prettyBooleanFormatter2(false) == "ложь")
            assert(Exercises.prettyBooleanFormatter2(777) == "777")
            assert(Exercises.prettyBooleanFormatter2("Shoot") == "Shoot")
        }
        'test_prettyBooleanFormatter3 - {
            assert(Exercises.prettyBooleanFormatter3(true) == "правда")
            assert(Exercises.prettyBooleanFormatter3(false) == "ложь")
            assert(Exercises.prettyBooleanFormatter3(777) == "777")
            assert(Exercises.prettyBooleanFormatter3("Shoot") == "Shoot")
        }
        'test_max1 - {
            intercept[IllegalArgumentException]{Exercises.max1(Seq.empty[Int])}
            assert(Exercises.max1(Seq(1, 1)) == 1)
            assert(Exercises.max1(Seq(5, -5)) == 5)
            assert(Exercises.max1(Seq(-1, -2)) == -1)
            assert(Exercises.max1(Seq(-1, 0)) == 0)
        }
        'test_max2 - {
            assert(Exercises.max2(Seq.empty[Int]) == Seq.empty[Int])
            assert(Exercises.max2(Seq(1, 1)) == Seq(1))
            assert(Exercises.max2(Seq(5, -5)) == Seq(5))
            assert(Exercises.max2(Seq(-1, -2)) == Seq(-1))
            assert(Exercises.max2(Seq(-1, 0)) == Seq(0))
        }
        'test_max3 - {
            assert(Exercises.max3(Seq()) == None)
            assert(Exercises.max3(Seq(1, 1)) == Option(1))
            assert(Exercises.max3(Seq(5, -5)) == Option(5))
            assert(Exercises.max3(Seq(-1, -2)) == Option(-1))
            assert(Exercises.max3(Seq(-1, 0)) == Option(0))
        }
        'test_sum1 - {
            assert(Exercises.sum1(5,5) == 10)
            assert(Exercises.sum1(-2,0) == -2)
            assert(Exercises.sum1(0,0) == 0)
            assert(Exercises.sum1(-3,-3) == -6)
        }
        'test_sum2 - {
            assert(Exercises.sum2(5,5) == 10)
            assert(Exercises.sum2(-2,0) == -2)
            assert(Exercises.sum2(0,0) == 0)
            assert(Exercises.sum2(-3,-3) == -6)
        }
        'test_sum3 - {
            assert(Exercises.sum3(5,5) == 10)
            assert(Exercises.sum3(-2,0) == -2)
            assert(Exercises.sum3(0,0) == 0)
            assert(Exercises.sum3(-3,-3) == -6)
        }
    }
}
