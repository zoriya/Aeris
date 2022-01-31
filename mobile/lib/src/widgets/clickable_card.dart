import 'package:flutter/material.dart';

// A Card that can be clicked
class ClickableCard extends StatelessWidget {
  ///The body of the card
  final Widget body;

  ///Tap callback
  final Function() onTap;

  ///Card elevation
  final double elevation;

  ///Card rounded corners
  final int borderRadius;

  ///The color of the card
  final Color? color;
  const ClickableCard(
      {Key? key,
      required this.body,
      required this.onTap,
      this.elevation = 40,
      this.color,
      this.borderRadius = 25})
      : super(key: key);

  @override
  Widget build(BuildContext context) {
    BorderRadius bordersRadius = const BorderRadius.all(Radius.circular(25));
    return Card(
        elevation: elevation,
        color: color,
        shape: RoundedRectangleBorder(borderRadius: bordersRadius),
        child: InkWell(onTap: onTap, borderRadius: bordersRadius, child: body));
  }
}
