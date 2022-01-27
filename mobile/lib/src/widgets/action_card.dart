import 'package:flutter/material.dart';

class ActionCard extends StatelessWidget {
  // Leading widget (like an icon) on the left
  final Widget leading;
  // Title, displayed at the center
  final String title;
  // Widge ton the right of the card
  final Widget trailing;
  const ActionCard(
      {Key? key,
      required this.leading,
      required this.title,
      required this.trailing})
      : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Card(
      elevation: 5,
      shape: const RoundedRectangleBorder(
          borderRadius: BorderRadius.all(Radius.circular(20))),
      child: Row(
        children: [
          Expanded(child: leading, flex: 2),
          Expanded(
              child: Padding(
                  padding: const EdgeInsets.only(top: 20, bottom: 20),
                  child: Text(
                    title,
                    style: TextStyle(
                        color: Theme.of(context).colorScheme.onSurface),
                    textAlign: TextAlign.center,
                  )),
              flex: 8),
          Expanded(child: trailing, flex: 2),
        ],
      ),
    );
  }
}
