const getLabel = (labelName) => {
  return labelName.length == 0
    ? null
    : GmailApp.getUserLabelByName(
        labelName.split("/").reduce((acc, x) => {
          if (GmailApp.getUserLabelByName(acc) == null) {
            GmailApp.createLabel(acc);
          }
          return [acc, x].join("/");
        }),
      );
};

const getMessage = (thread) => {
  const messages = GmailApp.getMessagesForThread(thread);
  return messages.length != 1
    ? messages[messages.length - 1] // NOTE: sending last
    : messages[0];
};

const getUnreadBillingMailsWith = (f) => {
  const alreadyReadLabel = "Kiirotori/TokyoGasAlreadyRead";
  const title = "【myTOKYOGAS】ご請求料金確定のお知らせ";
  const q = [
    "from:official-enews@mail.tokyo-gas.co.jp has:nouserlabels",
    title,
  ].join(" ");
  const url = "https://members.tokyo-gas.co.jp/services/mieru/total.html";

  const label = getLabel(alreadyReadLabel);
  for (let thread of GmailApp.search(q)) {
    f(getMessage(thread).getDate().getMonth() + 1, url);
    thread.addLabel(label);
  }
};

const getUnreadOneTimePassWith = (f) => {
  const alreadyReadLabel = "Kiirotori/TokyoGasAlreadyRead";
  const title = "【myTOKYOGAS】ワンタイムパスワードのお知らせ";
  const q = ["from:members@tokyo-gas.co.jp has:nouserlabels", title].join(" ");

  const label = getLabel(alreadyReadLabel);
  for (let thread of GmailApp.search(q)) {
    const body = getMessage(thread).getPlainBody();
    f(
      body.match(/【ワンタイムパスワード】[0-9]{6}/),
      body.match(/【有効期間】.*/),
    );
    thread.addLabel(label);
  }
};
