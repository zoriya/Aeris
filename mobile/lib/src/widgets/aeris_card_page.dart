import 'package:flutter/material.dart';

// Popup Card page
class AerisCardPage extends StatelessWidget {
  // Body of the card
  final Widget body;
  const AerisCardPage({Key? key, required this.body}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    const double radius = 30;
    return ClipRRect(
      borderRadius: const BorderRadius.only(
          topLeft: Radius.circular(radius), topRight: Radius.circular(radius)),
      child: Container(
          color: Colors.white,
          padding: const EdgeInsets.only(top: 40, bottom: 0),
          child: body),
    );
  }
}
