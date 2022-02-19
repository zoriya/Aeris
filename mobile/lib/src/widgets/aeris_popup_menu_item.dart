import 'package:flutter/material.dart';

///Class for item in aeris popup menu
class AerisPopupMenuItem extends PopupMenuItem {
  AerisPopupMenuItem(
      {Key? key,
      required BuildContext context,
      required IconData icon,
      required String title,
      required Object? value,
      bool selected = false,
      bool enabled = true})
      : super(
          key: key,
          value: value,
          enabled: enabled,
          child: Row(
            children: <Widget>[
              Expanded(
                flex: 1,
                child: selected
                  ? Icon(
                    Icons.done,
                    color: Theme.of(context).colorScheme.onSurface
                  )
                  : Container()
              ),
              Expanded(
                flex: 2,
                child: Container(
                    child: Icon(
                      icon,
                      color: Theme.of(context).colorScheme.onSurface,
                    ),
                    padding: const EdgeInsets.only(left: 10, right: 25)),
              ),
              Expanded(flex: 7, child: Text(title)),
            ],
          ),
        );
}
