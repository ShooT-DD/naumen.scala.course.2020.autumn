import utest._
import Exercises.Vector2D

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1,9) == 23)
            assert(Exercises.sumOfDivBy3Or5(1,3) == 3)
            assert(Exercises.sumOfDivBy3Or5(1,5) == 8)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2,5))
            assert(Exercises.primeFactor(98) == Seq(2,7))
            assert(Exercises.primeFactor(13) == Seq(13))
        }
        'test_sumScalars - {
            assert(Exercises.sumScalars(Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(2.0, 2.0)) == 16)
            assert(Exercises.sumScalars(Vector2D(0.0, 0.0), Vector2D(0.0, 0.0), Vector2D(0.0, 0.0), Vector2D(0.0, 0.0)) == 0)
        }
        'test_sumCosines - {
            assert(Exercises.sumCosines(Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(3.0, 2.0), Vector2D(3.0, 2.0)) == 2.0)
            assert(Exercises.sumCosines(Vector2D(1.0, 0.0), Vector2D(1.0, 0.0), Vector2D(2.0, 0.0), Vector2D(2.0, 0.0)) == 2)
        }
        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight() == Seq("Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium", "Uranium", "Gold", "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium", "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite"))
        }
    }
}

