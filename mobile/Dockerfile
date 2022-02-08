FROM androidsdk/android-30:latest

# Download Flutter SDK
RUN git clone https://github.com/flutter/flutter.git
ENV PATH "$PATH:/opt/android-sdk-linux/flutter/bin"

# Run basic check to download Dark SDK
RUN flutter doctor

COPY pubspec.* ./
RUN flutter pub get

COPY . .
# Generate traduction files
RUN fkutter gen-l10n
RUN flutter build apk lib/src/main.dart
CMD mv ./build/app/outputs/flutter-apk/app-release.apk /dist/aeris_android.apk
