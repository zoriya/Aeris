import 'package:flutter/material.dart';

///Class for item in aeris popup menu
class AerisPopupMenuItem extends PopupMenuItem {
  AerisPopupMenuItem(
      {Key? key,
      required BuildContext context,
      required IconData icon,
      required String title,
      required Object value,
      bool enabled = true})
      : super(
            key: key,
            value: value,
            enabled: enabled,
            child: Row(
              children: <Widget>[
                Container(
                    child: Icon(
                      icon,
                      color: Theme.of(context).colorScheme.onSurface,
                    ),
                    padding: const EdgeInsets.only(left: 10, right: 10)),
                Text(title),
              ],
            ));
}
