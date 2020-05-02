## Envelopes of epicycloids

# <https://www.johndcook.com/blog/2020/04/24/envelopes-of-epicycloids/>

ants = function (speed_a, speed_b, n = 200L) {
    ts = 1i * seq(0, 2 * pi, length.out = n)
    points_a = exp(ts * speed_a)
    points_b = exp(ts * speed_b)

    par(mar = rep(1, 4L))
    plot.new()
    plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
    segments(
        Re(points_a), Im(points_a),
        Re(points_b), Im(points_b),
        col = '#12345678',
        lwd = 2
    )
}

png('ants.png', width = 1500L, height = 1500L, res = 120L)
par(mfrow = c(2, 2))
ants(2, 4)
ants(2, 8)
ants(4, 10)
ants(4, 9, 300L)
dev.off()

# ants(3, 5)
# ants(3, 9)
# ants(3, 7)
