import 'package:flutter/material.dart';
import 'package:modal_bottom_sheet/modal_bottom_sheet.dart';

/// Function to show a Card page
showAerisCardPage(BuildContext context, Widget Function(BuildContext) builder) {
  return showCupertinoModalBottomSheet(
      context: context,
      builder: builder,
      backgroundColor: Theme.of(context).colorScheme.surface);
}

///Popup Card page
class AerisCardPage extends StatefulWidget {
  ///Body of the card
  final Widget body;
  const AerisCardPage({Key? key, required this.body}) : super(key: key);

  @override
  State<AerisCardPage> createState() => _AerisCardPageState();
}

class _AerisCardPageState extends State<AerisCardPage> {
  /// Know that the Card page is still the current/active one, and it has not been poped.
  bool active = true;

  @override
  Widget build(BuildContext context) {
    const Radius radius = Radius.circular(100);
    return Scaffold(
        body: CupertinoScaffold(
            transitionBackgroundColor: Theme.of(context).colorScheme.surface,
            topRadius: radius,
            body: Padding(
              padding: const EdgeInsets.only(left: 10, right: 10),
              child: ListView(
                physics: const BouncingScrollPhysics(),
                controller: ModalScrollController.of(context),
                children: [
                  Padding(
                    padding: const EdgeInsets.only(top: 30.0),
                    child: Align(
                      child: Container(
                        width: 30,
                        height: 4,
                        decoration: BoxDecoration(
                          borderRadius:
                              const BorderRadius.all(Radius.circular(100)),
                          color: Theme.of(context)
                              .colorScheme
                              .primary
                              .withAlpha(70),
                        ),
                        alignment: Alignment.center,
                      ),
                    ),
                  ),
                  Container(
                    child: const AerisPageCloseButton(),
                    alignment: Alignment.centerRight,
                  ),
                  Padding(
                      padding: const EdgeInsets.only(left: 20, right: 20),
                      child: widget.body)
                ],
              ),
            )));
  }
}

///Close button for aeris page
class AerisPageCloseButton extends StatelessWidget {
  const AerisPageCloseButton({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return ElevatedButton(
      onPressed: () => Navigator.pop(context),
      child: Icon(Icons.close,
          color: Theme.of(context).colorScheme.onSecondary, size: 20),
      style: ElevatedButton.styleFrom(
          elevation: 0,
          shape: const CircleBorder(),
          primary: Theme.of(context).colorScheme.secondary),
    );
  }
}
