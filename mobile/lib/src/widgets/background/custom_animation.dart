import 'package:flutter/material.dart';

import '../../../aeris.dart';

class CustomAnimation extends StatefulWidget {

  final double topOffset, leftOffset;
  final AnimationController parent;
  final double begin, end;
  final Color gradientLeft, gradientRight;
  final bool listener;
  final Curve curve;

  const CustomAnimation(
      {Key? key,
        required this.begin,
        required this.end,
        required this.curve,
        required this.parent,
        required this.topOffset,
        required this.leftOffset,
        this.gradientLeft = Colors.black,
        this.gradientRight = Colors.white,
        this.listener = false
      }
  ) : super(key: key);

  @override
  _CustomAnimationState createState() => _CustomAnimationState();
}

class _CustomAnimationState extends State<CustomAnimation> with TickerProviderStateMixin {

  late Animation<double> newAnim;

  @override
  void initState() {
    super.initState();
    newAnim = Tween<double>(begin: widget.begin, end: widget.end).animate(
      CurvedAnimation(
        parent: widget.parent,
        curve: widget.curve
      )
    )
    ..addListener(() {
      setState(() {});
    });

    if (widget.listener == true) {
      newAnim.addStatusListener((status) {
        if (status == AnimationStatus.completed) {
          widget.parent.reverse();
        } else if (status == AnimationStatus.dismissed) {
          widget.parent.forward();
        }
      });
    }
  }

  @override
  void dispose() {
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    Size size = MediaQuery.of(context).size;

    return Positioned(
      top: size.height * .98,
      left: size.width * .1,
      child: CustomPaint(
        painter: AnimPainter(
          radius: newAnim.value,
          gradientLeft: widget.gradientLeft,
          gradientRight: widget.gradientRight
        )
      )
    );
  }
}
