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
        title: Text("Warning",
            style: TextStyle(color: Theme.of(context).colorScheme.error)),
        content: Text(message),
        actions: [
          Row(
            mainAxisAlignment: MainAxisAlignment.spaceEvenly,
            children: [
              ElevatedButton(
                style: ElevatedButton.styleFrom(
                    primary: Theme.of(context).colorScheme.primaryContainer),
                onPressed: () => Navigator.pop(context),
                child: const Text('Cancel'),
              ),
              ElevatedButton(
                  style: ElevatedButton.styleFrom(
                      primary: Theme.of(context).colorScheme.error),
                  onPressed: () => {
                        onAccept(),
                        Navigator.pop(context),
                      },
                  child: Text(warnedAction)),
            ],
          ),
        ]);
  }
}
