import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'dart:async';

import '../../aeris.dart';

class StartupPage extends StatefulWidget {

  const StartupPage({Key? key}) : super(key: key);

  @override
  _StartupPageState createState() => _StartupPageState();
}

class _StartupPageState extends State<StartupPage> with TickerProviderStateMixin {

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
    topAnimController.dispose();
    bottomAnimController.dispose();
    super.dispose();
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
                gradientRight: const Color(0xffC43990)
              )
            )
          ),
          Positioned(
            top: size.height * .98,
            left: size.width * .1,
            child: CustomPaint(
              painter: AnimPainter(
                radius: botLeftAnimation.value - 30,
                gradientLeft: const Color.fromRGBO(240, 98, 146, 1),
                gradientRight: const Color(0xffC43990)
              )
            )
          ),
          Positioned(
            top: size.height * .5,
            left: size.width * (topLeftAnimation.value + .8),
            child: CustomPaint(
              painter: AnimPainter(
                radius: 30,
                gradientLeft: const Color.fromRGBO(240, 98, 146, 1),
                gradientRight: const Color(0xffC43990)
              )
            )
          ),
          Positioned(
            top: size.height * botRightAnimation.value,
            left: size.width * (topRightAnimation.value + .1),
            child: CustomPaint(
              painter: AnimPainter(
                radius: 60,
                gradientLeft: const Color.fromRGBO(240, 98, 146, 1),
                gradientRight: const Color(0xffC43990)
              )
            )
          ),
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
          ),
          Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              Align(
                alignment: Alignment.bottomCenter,
                child: ElevatedButton(
                  style: ElevatedButton.styleFrom(
                    textStyle: const TextStyle(
                      fontSize: 20
                    ),
                  primary: Theme.of(context).colorScheme.primary,
                ),
                  onPressed: () {
                    if (kDebugMode) {
                      print("On clique sur 'se connecter'!");
                    }
                  },
                  child: const Tooltip(
                    message: 'Connexion',
                    child: Text("Se connecter")
                  ),
                ),
              ),
              OverlayedText(
                text: "Aeris your new Action Dashboard!",
                overlayedColor: Colors.black,
                textColor: Theme.of(context).colorScheme.secondary,
                fontSize: 20,
                strokeWidth: 2.15
              )
            ]
          )
        ],
      )
    );
  }
}