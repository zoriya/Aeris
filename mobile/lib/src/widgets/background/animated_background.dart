import 'package:flutter/material.dart';
import 'dart:async';

import '../../../aeris.dart';

class AnimatedBackground extends StatefulWidget {
  const AnimatedBackground({Key? key}) : super(key: key);

  @override
  _AnimatedBackgroundState createState() => _AnimatedBackgroundState();
}

class _AnimatedBackgroundState extends State<AnimatedBackground> with TickerProviderStateMixin {
  late AnimationController topAnimController;
  late AnimationController bottomAnimController;
  late Animation<double> topRightAnimation;
  late Animation<double> topLeftAnimation;
  late Animation<double> botRightAnimation;
  late Animation<double> botLeftAnimation;

  @override
  void initState() {
    super.initState();

    topAnimController = bottomAnimController = AnimationController(
      vsync: this,
      duration: const Duration(
        seconds: 5
      )
    );

    topRightAnimation = Tween<double>(begin: .1, end: .15).animate(
      CurvedAnimation(
        parent: topAnimController,
        curve: Curves.easeInOut
      )
    )
    ..addListener(() {
      setState(() {});
    })
    ..addStatusListener((status) {
      if (status == AnimationStatus.completed) {
        topAnimController.reverse();
      } else if (status == AnimationStatus.dismissed) {
        topAnimController.forward();
      }
    });

    topLeftAnimation = Tween<double>(begin: .02, end: .04).animate(
        CurvedAnimation(
            parent: topAnimController,
            curve: Curves.easeInOut
        )
    )
    ..addListener(() {
      setState(() {});
    });
    
    botRightAnimation = Tween<double>(begin: .41, end: .38).animate(CurvedAnimation(
      parent: bottomAnimController,
      curve: Curves.easeInOut,
    ))
      ..addListener(() {
        setState(() {});
      })
      ..addStatusListener((status) {
        if (status == AnimationStatus.completed) {
          bottomAnimController.reverse();
        } else if (status == AnimationStatus.dismissed) {
          bottomAnimController.forward();
        }
      });
    botLeftAnimation = Tween<double>(begin: 170, end: 190).animate(
      CurvedAnimation(
        parent: bottomAnimController,
        curve: Curves.easeInOut,
      ),
    )..addListener(() {
      setState(() {});
    });

    Timer(const Duration(milliseconds: 2500), () {
      topAnimController.forward();
    });

    bottomAnimController.forward();
  }

  @override
  void dispose() {
    if (mounted) {
      // topAnimController.dispose();
      bottomAnimController.dispose();
      super.dispose();
    }
  }

  @override
  Widget build(BuildContext context) {
    Size size = MediaQuery.of(context).size;
    return Scaffold(
      backgroundColor: Theme.of(context).colorScheme.primary,
      body: Stack(
        children: <Widget>[
          Positioned(
            top: size.height * (topLeftAnimation.value + 0.5),
            left: size.width * .21,
            child: CustomPaint(
              painter: AnimPainter(
                radius: 50,
                gradientLeft: const Color.fromRGBO(240, 98, 146, 1),
                gradientRight: const Color(0xffC43990)))),
          Positioned(
            top: size.height * .98,
            left: size.width * (-0.05),
            child: CustomPaint(
              painter: AnimPainter(
                radius: botLeftAnimation.value - 30,
                gradientLeft: const Color.fromRGBO(240, 98, 146, 1),
                gradientRight: const Color(0xffC43990)))),
          Positioned(
            top: size.height * .5,
            left: size.width * (topLeftAnimation.value + .8),
            child: CustomPaint(
              painter: AnimPainter(
                radius: 30,
                gradientLeft: const Color.fromRGBO(240, 98, 146, 1),
                gradientRight: const Color(0xffC43990)))),
          Positioned(
            top: size.height * botRightAnimation.value,
            left: size.width * (topRightAnimation.value + .1),
            child: CustomPaint(
              painter: AnimPainter(
                radius: 60,
                gradientLeft: const Color.fromRGBO(240, 98, 146, 1),
                gradientRight: const Color(0xffC43990)))),
          Positioned(
            top: size.height * .1,
            left: size.width * .8,
            child: CustomPaint(
              painter: AnimPainter(
                radius: botLeftAnimation.value,
                gradientLeft: const Color.fromRGBO(240, 98, 146, 1),
                gradientRight: const Color(0xffC43990)
              )
            )
          )
        ],
      )
    );
  }
}
