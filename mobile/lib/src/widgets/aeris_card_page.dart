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
          padding: const EdgeInsets.only(top: 30, bottom: 0),
          child: Column(
            children: [
              Container(
                child: const AerisPageCloseButton(),
                alignment: Alignment.centerRight,
              )
            ],
          )),
    );
  }
}

// Close button for aeris page
class AerisPageCloseButton extends StatelessWidget {
  const AerisPageCloseButton({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return ElevatedButton(
      onPressed: () => Navigator.pop(context),
      child:
          Icon(Icons.close, color: Theme.of(context).colorScheme.onSecondary),
      style: ElevatedButton.styleFrom(
          elevation: 0,
          shape: const CircleBorder(),
          padding: const EdgeInsets.all(5),
          primary: Theme.of(context).colorScheme.secondary),
    );
  }
}
