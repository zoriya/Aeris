import 'package:flutter/material.dart';

// Dialog for a warning (for example, a deletion)
class WarningDialog extends StatelessWidget {
  // The content of the dialog
  final String message;
  // The name of the action the warning is for
  final String warnedAction;
  // The action to execute once the warning was accepted
  final void Function() onAccept;

  const WarningDialog(
      {Key? key,
      required this.message,
      required this.onAccept,
      required this.warnedAction})
      : super(key: key);

  @override
  Widget build(BuildContext context) {
    return AlertDialog(
        title: const Text("Warning"),
        content: Text(message),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context, 'Cancel'),
            child: const Text('Cancel'),
          ),
          TextButton(
              onPressed: () => {Navigator.pop(context, 'Cancel'), onAccept},
              child: Text(warnedAction)),
        ]);
  }
}
