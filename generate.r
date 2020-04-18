# After <https://twitter.com/georgemsavva/status/1250914089028763649>.

f = function (t, sign, circles) {
    series = 0 : circles
    rot = function (t) sum(exp(1i * pi * t * (-5) ^ series) / ((sign * 2.5) ^ series))
    vapply(t, rot, complex(1L))
}

draw_radii = function (pts) {
    pts = c(0, pts)
    x = vapply(pts, Re, numeric(1L))
    y = vapply(pts, Im, numeric(1L))
    lines(x, y, lwd = 2L, col = 'lightgray')
    points(x, y, pch = 20, col = 'lightgray')
}

plot_rosetta = function (ts, z1, z2, up_to, circles) {
    is = seq_len(up_to)
    z1 = z1[is]
    z2 = z2[is]

    par(bg = '#0D2A35', mar = c(2, 2, 2, 2))
    plot.new()
    plot.window(xlim = c(-1.6, 1.6), ylim = c(-1.6, 1.6))
    grid(col = 'lightgray')
    lines(Re(z1), Im(z1), col = '#E36209')
    lines(Re(z2), Im(z2), col = '#46AFF8')

    pts1 = vapply(0 : circles, f, complex(1L), t = ts[up_to], sign = 1)
    pts2 =  vapply(0 : circles, f, complex(1L), t = ts[up_to], sign = -1)

    draw_radii(pts1)
    draw_radii(pts2)
}

# Use cosine interpolation to smooth the beginning and end of the animation.
cosine_interpolate = function (t, to) {
    as.integer(floor(to * (0.5 - cos(pi * (t - 1L) / to) / 2))) + 1L
}

# Animate the construction of the figure by circumscribing the circular input
# coordinates from 0 to π progressively. The figure is constructed by connecting
# 100,000 individual points but since animating 100,000 steps would be slow and
# unnecessary to get a fluid animation, we only animate 1000 frames.
figure_points = 100000L
frames = 1000L

circles = 4L
ts = seq(0, 2, length.out = figure_points)
z1data = f(ts, 1, circles)
z2data = f(ts, -1, circles)

dir.create('img', showWarnings = FALSE)

for (frame in seq(frames)) {
    t = (frame - 1L) * figure_points / frames + 1L
    idx = cosine_interpolate(t, length(ts))
    cat(sprintf('\rt = %d (%.1f%%)', idx, idx / length(ts) * 100))
    png(sprintf('img/rosetta_%03d.png', frame))
    plot_rosetta(ts, z1data, z2data, up_to = idx, circles = circles)
    dev.off()
}

cat(sprintf('\rt = %d (%.1f%%)\n', figure_points, 100))
