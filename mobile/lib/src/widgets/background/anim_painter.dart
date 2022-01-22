import 'package:flutter/material.dart';

class AnimPainter extends CustomPainter {
  final double radius;
  final Color gradientLeft, gradientRight;

  AnimPainter(
      {required this.radius,
      this.gradientLeft = Colors.black,
      this.gradientRight = Colors.white});

  @override
  void paint(Canvas canvas, Size size) {
    Path shadowPath = Path();
    Offset center = Offset(size.width / 2, size.height / 2);

    shadowPath.addOval(Rect.fromCircle(center: center, radius: radius));
    final paint = Paint()
      ..shader = LinearGradient(
              colors: [gradientLeft, gradientRight],
              begin: Alignment.topLeft,
              end: Alignment.bottomRight)
          .createShader(
              Rect.fromCircle(center: const Offset(0, 0), radius: radius));
    canvas.drawShadow(shadowPath, Colors.black, 15, true);
    canvas.drawCircle(Offset.zero, radius, paint);
  }

  @override
  bool shouldRepaint(covariant CustomPainter oldDelegate) {
    return true;
  }
}
