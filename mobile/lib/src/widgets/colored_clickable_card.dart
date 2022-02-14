import 'package:flutter/material.dart';
import 'package:aeris/src/widgets/clickable_card.dart';

/// Clickable card with simple text and color
class ColoredClickableCard extends StatelessWidget {
  /// text of the card
  final String text;

  /// Color of the card
  final Color color;

  final void Function() onTap;
  const ColoredClickableCard(
      {Key? key, required this.text, required this.color, required this.onTap})
      : super(key: key);

  @override
  Widget build(BuildContext context) => ClickableCard(
      color: color,
      elevation: 5,
      body: Container(
          child: Text(
            text,
            textAlign: TextAlign.center,
            style: TextStyle(
                color: Theme.of(context).colorScheme.onSecondary,
                fontSize: 15,
                fontWeight: FontWeight.w600),
          ),
          width: double.infinity,
          padding: const EdgeInsets.only(top: 15, bottom: 15)),
      onTap: onTap);
}
