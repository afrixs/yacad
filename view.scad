fm = [[1, 0, 0, 0],
              [0, 0, 1, 0],
              [0, 1, 0, 0],
              [0, 0, 0, 1]];

translate([3, 0, 0])
rotate([0, 0, 180])
multmatrix(m=fm)
import("testm-svx.stl", convexity=3);

translate([0, 3, 0])
rotate([0, 0, -90])
multmatrix(m=fm)
import("testm-svx-from-svx.stl", convexity=3);

translate([-3, 0, 0])
intersection() {
import("testm-stl.stl", convexity=3);
import("testm-stl-from-svx.stl", convexity=3);
}

translate([-0.5, -1, -1.2])
multmatrix(m=fm)
import("testm-cube-dil.stl", convexity=3);