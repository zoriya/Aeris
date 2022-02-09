import 'package:flutter/material.dart';

/// [Widget] used to show a text with colored border
class OverlayedText extends StatelessWidget {

  /// Text to display
  final String text;

  /// Overlay color
  final Color overlayedColor;

  /// Base text color
  final Color textColor;

  /// Font size
  final double fontSize;

  /// Border width
  final double strokeWidth;

  const OverlayedText({
    Key? key,
    required this.text,
    required this.overlayedColor,
    required this.textColor,
    required this.fontSize,
    this.strokeWidth = 3.0
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Container(
      alignment: Alignment.center,
      child: Stack(
        children: <Widget>[
          Text(text,
            textAlign: TextAlign.center,
            style: TextStyle(
              fontSize: fontSize,
              foreground: Paint()
                ..style = PaintingStyle.stroke
                ..strokeWidth = strokeWidth
                ..color = overlayedColor,
            ),
          ),
          Text(text,
            textAlign: TextAlign.center,
            style: TextStyle(
              fontSize: fontSize,
              color: textColor
            ),
          )
        ]
      )
    );
  }
}
