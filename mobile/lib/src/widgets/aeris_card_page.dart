import 'package:flutter/material.dart';

///Popup Card page
class AerisCardPage extends StatelessWidget {
  ///Body of the card
  final Widget body;
  const AerisCardPage({Key? key, required this.body}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    const double radius = 30;
    return Scaffold(
      resizeToAvoidBottomInset: false,
      backgroundColor: Colors.transparent,
      body: SafeArea(
        child: ClipRRect(
          borderRadius: const BorderRadius.only(
              topLeft: Radius.circular(radius),
              topRight: Radius.circular(radius)),
          child: GestureDetector(
            onVerticalDragUpdate: (details) {
              if (details.delta.dy > 8) {
                Navigator.of(context).pop();
              }
            },
            child: Container(
                color: Theme.of(context).colorScheme.surface,
                padding: const EdgeInsets.only(left: 15, right: 15, top: 30),
                child: Column(
                  children: [
                    Container(
                      width: 30,
                      height: 4,
                      decoration: BoxDecoration(
                        borderRadius:
                            const BorderRadius.all(Radius.circular(100)),
                        color:
                            Theme.of(context).colorScheme.primary.withAlpha(70),
                      ),
                      alignment: Alignment.center,
                    ),
                    Container(
                      child: const AerisPageCloseButton(),
                      alignment: Alignment.centerRight,
                    ),
                    Expanded(
                        child: Padding(
                            padding: const EdgeInsets.only(left: 20, right: 20),
                            child: body))
                  ],
                )),
          ),
        ),
      ),
    );
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
