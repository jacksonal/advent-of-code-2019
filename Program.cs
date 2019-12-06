using System;
using System.Collections.Generic;
using System.Linq;

namespace adventofcode2019
{
    class Program
    {
        static void Main(string[] args)
        {
            var lines = Inputs.input_day3.Split('\n', StringSplitOptions.RemoveEmptyEntries);
            var wire1 = BuildSegments(GetWirePoints(lines.First().Split(',')));
            var wire2 = BuildSegments(GetWirePoints(lines.Last().Split(',')));

            var intersectCount = 0;
            var intersectDistance = Int32.MaxValue;
            var intersectWirePathDistance = Int32.MaxValue;
            var wire1Length = 0;
            
            foreach(var seg1 in wire1)
            {
                var wire2Length = 0;
                foreach (var seg2 in wire2)
                {
                    
                    if(Intersects(seg1, seg2))
                    {
                        var intersectPoint = GetIntersectPoint(seg1, seg2);
                        if (GetDistanceFromOrigin(intersectPoint) != 0)
                        {
                            if (GetDistanceFromOrigin(intersectPoint) < intersectDistance)
                            {
                                intersectDistance = GetDistanceFromOrigin(intersectPoint);
                                Console.WriteLine(intersectDistance);
                            }
                            var wire1IntersectLength = GetLength(seg1.Points[0], intersectPoint);
                            var wire2IntersectLength = GetLength(seg2.Points[0], intersectPoint);
                            if((wire1IntersectLength + wire2IntersectLength + wire1Length + wire2Length) < intersectWirePathDistance)
                            {
                                intersectWirePathDistance = wire1IntersectLength + wire2IntersectLength + wire1Length + wire2Length;
                                Console.WriteLine($"Total Wire Length: {intersectWirePathDistance}");
                            }
                        }
                    }
                    wire2Length += seg2.Length;
                }
                wire1Length += seg1.Length;
            }
            Console.WriteLine(intersectDistance);
        }

        private static int GetLength(Point p1, Point p2)
        {
            return Math.Abs(p1.X - p2.X) + Math.Abs(p1.Y - p2.Y);
        }

        private static int GetDistanceFromOrigin(Point intersectPoint)
        {
            return Math.Abs(intersectPoint.X) + Math.Abs(intersectPoint.Y);
        }

        private static Point GetIntersectPoint(Segment seg1, Segment seg2)
        {
            if(seg1.Orientation == 'H')
            {
                return new Point(seg2.Points[0].X, seg1.Points[0].Y);
            }
            else
            {
                return new Point(seg1.Points[0].X, seg2.Points[0].Y);
            }
        }

        private static IEnumerable<Segment> BuildSegments(IEnumerable<Point> points)
        {
            var start = new Point(0, 0);
            foreach(var p in points)
            {
                yield return new Segment(start, p);
                start = p;
            }
        }

        private static IEnumerable<Point> GetWirePoints(string[] wireInstructions)
        {
            var currentX = 0;
            var currentY = 0;
            foreach (var instruction in wireInstructions)
            {
                var distance = Convert.ToInt32(instruction.Substring(1));
                switch (instruction[0])
                {
                    case 'U':
                        currentY += distance;
                        break;
                    case 'D':
                        currentY -= distance;
                        break;
                    case 'L':
                        currentX -= distance;
                        break;
                    case 'R':
                        currentX += distance;
                        break;
                    default:
                        throw new ArgumentException();
                }
                yield return new Point(currentX, currentY);
            }
        }

        public static bool Intersects (Segment first, Segment second)
        {
            if (AreParallel(first, second))
                return false;
            else
            {
                return first.Points[0].IsYBetween(second) && second.Points[0].IsXBetween(first) ||
                    first.Points[0].IsXBetween(second) && second.Points[0].IsYBetween(first);
            }
        }

        private static bool AreParallel(Segment first, Segment second)
        {
            return first.Orientation == second.Orientation;
        }
    }

    public class Segment
    {

        public Segment(Point start, Point end)
        {
            Points = new Point[2];
            Points[0] = start;
            Points[1] = end;
        }

        public char Orientation { get { return GetOrientation(); } }
        public int Length { get { return GetLength(); } }

        private int GetLength()
        {
            return Math.Abs(Points[0].X - Points[1].X) + Math.Abs(Points[0].Y - Points[1].Y);
        }

        public Point[] Points { get; private set; }

        private char GetOrientation()
        {
            if (Points[0].X == Points[1].X)
                return 'V';
            else if (Points[0].Y == Points[1].Y)
                return 'H';
            else
                return ' ';
        }
    }

    public class Point
    {
        public Point(int currentX, int currentY)
        {
            this.X = currentX;
            this.Y = currentY;
        }

        public int X { get; internal set; }
        public int Y { get; internal set; }

        public bool IsXBetween(Segment seg)
        {
            return (seg.Points[0].Y <= Y && seg.Points[1].Y >= Y) || (seg.Points[0].Y >= Y && seg.Points[1].Y <= Y);
        }
        public bool IsYBetween(Segment seg)
        {
            return (seg.Points[0].X <= X && seg.Points[1].X >= X) || (seg.Points[0].X >= X && seg.Points[1].X <= X);
        }
    }
}
